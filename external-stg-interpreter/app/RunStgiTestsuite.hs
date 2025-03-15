{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import Control.Monad
import Data.Containers.ListUtils
import System.Directory
import System.FilePath
import System.FilePath.Find
import System.Process
import Text.Printf

import Control.Concurrent.Async.Pool
import GHC.Conc (getNumProcessors)
import System.TimeIt
import System.Timeout
import System.Exit
import System.IO
import System.Environment

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Text.PrettyPrint.ANSI.Leijen hiding ((</>), (<$>))


{- testsuite compilation / preparation
  COMPILE TESTS:
    (PATH=/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin:$PATH make CLEANUP=0 THREADS=12 RUNNABLE_ONLY=1 EXTRA_HC_OPTS='-fPIC')
-}

{-
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/'
-}

{-
  TODO
    done - save exit code
    clean ext stg work dir
    save test results to files
-}


maxMicroSec :: Int
maxMicroSec = 3 * 60 * 1000 * 1000

findTests :: [FilePath] -> IO [FilePath]
findTests paths = do
  l <- forM paths $ \path0 -> do
    path <- pathIsSymbolicLink path0 >>= \case
      True  -> getSymbolicLinkTarget path0
      False -> pure path0
    printf "Scanning: %s\n" path
    let followSymLink = fromMaybe <$> fileName <*> readLink
    find always (followSymLink ~~? "*.run.stderr") path
  let tests = nubOrd $ concat l
  printf "Found: %d tests\n" (length tests)
  pure tests

data TestResult
  = OK      FilePath
  | Fail    FilePath (ExitCode, ExitCode) (ByteString, ByteString) (ByteString, ByteString) -- stdout, stderr
  | Error   FilePath String
  | Skip    FilePath
  | Timeout FilePath
  deriving Show

report :: TestResult -> IO TestResult
report t = do
  case t of
    OK f      -> printf "%s %s\n" (show $ green "OK") f
    Error f e -> printf "%s %s\n%s\n" (show $ red "ERROR") (show . red $ text f) (show . red $ text e)
    Skip f    -> printf "%s %s\n" (show $ cyan "SKIP") (show . cyan $ text f)
    Timeout f -> printf "%s %s\n" (show $ red "TIMEOUT") (show . red $ text f)
    Fail f (expectedExitCode, exitCode) (expectedStdout, out) (expectedStderr, err) -> do
      let exitCodeMsg = if expectedExitCode == exitCode then "" else unlines
            [ "  exitcode mismatch"
            , "  expected: " ++ show expectedExitCode
            , "  got     : " ++ show exitCode
            ]
          stdoutMsg = if expectedStdout == out then "" else unlines
            [ "  stdout mismatch"
            , "  expected: " ++ show expectedStdout
            , "  got     : " ++ show out
            ]
          stderrMsg = if expectedStderr == err then "" else unlines
            [ "  stderr mismatch"
            , "  expected: " ++ show expectedStderr
            , "  got     : " ++ show err
            ]
          msg = unlines $ filter (not . null) [exitCodeMsg, stdoutMsg, stderrMsg]
      printf "%s %s\n%s\n" (show $ red "FAIL") (show $ red $ text f) (show $ red $ text msg)
  pure t

runTestProcess :: FilePath -> String -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runTestProcess path cmd args input = do
  let stdinPath   = path -<.> ".stgi.stdin"
      stdoutPath  = path -<.> ".stgi.stdout"
      stderrPath  = path -<.> ".stgi.stderr"
  BS8.writeFile stdinPath input
  exitCode <- withFile stdinPath ReadMode $ \hIn -> do
    withFile stdoutPath WriteMode $ \hOut -> do
      withFile stderrPath WriteMode $ \hErr -> do
        let procSpec = (proc cmd args)
              { std_in  = UseHandle hIn
              , std_out = UseHandle hOut
              , std_err = UseHandle hErr
              }
        withCreateProcess procSpec $ \_ _ _ procHandle -> waitForProcess procHandle
  outData <- BS8.readFile stdoutPath
  errData <- BS8.readFile stderrPath
  pure (exitCode, outData, errData)

readTestOpts :: FilePath -> IO (Bool, Bool)
readTestOpts optsPath = do
  opts <- Map.fromList . read <$> readFile optsPath
  pure . fromJust $ (,) <$> Map.lookup "ignore_stdout" opts <*> Map.lookup "ignore_stderr" opts

runTest :: Set FilePath -> FilePath -> IO TestResult
runTest skipSet stderrPath = do
  let stdoutPath    = stderrPath -<.> ".stdout"
      stdinPath     = stderrPath -<.> ".stdin"
      argsPath      = stderrPath -<.> ".args"
      optsPath      = stderrPath -<.> ".opts"
      exitcodePath  = stderrPath -<.> ".exitcode"
      testName      = takeFileName . dropExtension $ dropExtension stderrPath
      testDir       = takeDirectory stderrPath
      requiredFiles = [stdoutPath, stderrPath, exitcodePath]

  missingFiles <- fmap catMaybes . forM requiredFiles $ \fname -> doesFileExist fname >>= \case
    True  -> pure Nothing
    False -> pure . Just $ "missing required file: " ++ fname

  stgApps <- find (depth ==? 0) (fileName ~~? (testName ++ ".*_ghc_stgapp")) testDir
  case stgApps of
    [ghcstgappPath]
      | Set.member ghcstgappPath skipSet
      -> do
        report $ Skip ghcstgappPath

      | missingFiles /= []
      -> do
        report $ Error ghcstgappPath $ unlines missingFiles

      | otherwise
      -> do
          testArgOpt <- doesFileExist argsPath >>= \case
            False -> pure []
            True  -> pure ["--args-file", argsPath, "--ignore-rts-args"]

          testInput <- doesFileExist stdinPath >>= \case
            False -> pure ""
            True  -> BS8.readFile stdinPath

          (ignoreStdout, ignoreStderr) <- readTestOpts optsPath
          expectedStderr <- BS8.readFile stderrPath
          expectedStdout <- BS8.readFile stdoutPath
          expectedExitCode <- (read <$> readFile exitcodePath) >>= \case
            0 -> pure ExitSuccess
            n -> pure (ExitFailure n)

          mResult <- timeout maxMicroSec $ runTestProcess stderrPath "ext-stg-interpreter"
            (["--RTS" -- HINT: pass RTS options to the interpeter (bypass native RTS)
            , "-q"
            , "--cwd"
            ] ++ testArgOpt ++ [ghcstgappPath]
            )
            testInput
          case mResult of
            Nothing -> report $ Timeout ghcstgappPath
            Just (exitCode, out, err) -> do
              if expectedExitCode == exitCode && (expectedStdout == out || ignoreStdout) && (expectedStderr == err || ignoreStderr)
                then report $ OK ghcstgappPath
                else report $ Fail ghcstgappPath (expectedExitCode, exitCode) (expectedStdout, out) (expectedStderr, err)

    []  -> report $ Error "" $ "missing ghc_stgapp: " ++ testDir </> testName ++ ".*_ghc_stgapp"
    l   -> report $ Error "" $ "ambiguous ghc_stgapp: " ++ unwords l

main = do
  scanList <- getArgs >>= \case
    []  -> pure testPaths
    l   -> pure l
  testList <- findTests scanList >>= mapM makeAbsolute

  cpuCount <- getNumProcessors
  result <- timeItNamed "run tests in the interpreter" $ do
    withTaskGroup cpuCount $ \g -> do
      mapTasks g [runTest skipSet test | test <- testList]

  let total = length result
      log :: String -> [a] -> IO ()
      log n l = do
        let len = length l
            r   = fromIntegral len / fromIntegral total * 100:: Double
        printf "%-8s : %-4d (%.1f%%)\n" n len r

  log "TOTAL"   result
  putStrLn ""

  log "OK"      [() | OK{} <- result]
  log "FAIL"    [() | Fail{} <- result]
  log "ERROR"   [() | Error{} <- result]
  log "SKIP"    [() | Skip{} <- result]
  log "TIMEOUT" [() | Timeout{} <- result]


testPaths1 = ["/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T18527.run"]

testPaths :: [FilePath]
testPaths =
  [ "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ado/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/array/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/arrows/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/boxy/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/cpranal/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/deriving/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/deSugar/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/determinism/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/driver/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/gadt/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/generics/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/indexed-types/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/lib/integer/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/array/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/process/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/mdo/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/overloadedlists/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/overloadedrecflds/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/overloadedstrings/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/parser/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/patsyn/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/polykinds/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quantified-constraints/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quasiquotation/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quotes/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rebindable/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/safeHaskell/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplStg/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/stranal/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/th/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/typecheck/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/callarity/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/"
--  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/"
  ]

skipSet :: Set FilePath
skipSet = Set.fromList $ [] -- ++ skip_set ++ skip_fail ++ skip_timeout

skip_set :: [FilePath]
skip_set =
  -- has stubs
  [ "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun078.run/cgrun078.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/driver/T16737.run/T16737.o_ghc_stgapp"

  -- has program args

  -- missing feature Static Pointers (cloud haskell) ; undefined symbol: hs_spt_lookup
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/CgStaticPointers.run/CgStaticPointers.o_ghc_stgapp"

  -- not enough memory
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/space_leaks/space_leak_001.run/space_leak_001.o_ghc_stgapp"

  -- needs STM primops
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T12852.run/T12852.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T16066.run/T16066.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/stranal/should_run/T14171.run/T14171.o_ghc_stgapp"

  -- needs RTS option parsing and heap mem limit support
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/simplrun010.run/simplrun010.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T10830.run/T10830.o_ghc_stgapp"

  -- needs detection of MVar blocked indefinitely
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/mdo/should_fail/mdofail006.run/mdofail006.o_ghc_stgapp"

  -- FFI symbol
      -- forkProcess
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/libposix/posix014.run/posix014.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/libposix/posix004.run/posix004.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/forkprocess01.run/forkprocess01.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/T1185.run/T1185.o_ghc_stgapp"
      -- rtsTimerSignal
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/libposix/posix009.run/posix009.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/libposix/posix006.run/posix006.o_ghc_stgapp"
      -- shutdownHaskellAndSignal
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/topHandler03.run/topHandler03.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/topHandler02.run/topHandler02.o_ghc_stgapp"
      -- hs_free_stable_ptr
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/stableptr005.run/stableptr005.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/stableptr004.run/stableptr004.o_ghc_stgapp"
      -- getMonotonicNSec
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/hWaitForInput-accurate-socket.run/hWaitForInput-accurate-socket.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/hWaitForInput-accurate-pipe.run/hWaitForInput-accurate-pipe.o_ghc_stgapp"
      -- getProcessElapsedTime\n
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T8684.run/T8684.o_ghc_stgapp"
      -- externalPutMVar
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/hs_try_putmvar002.run/hs_try_putmvar002.o_ghc_stgapp"

  -- bug
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/process/tests/process011.run/process011.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/topHandler01.run/topHandler01.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hReady002.run/hReady002.o_ghc_stgapp"
      --  called at lib/Stg/Interpreter/Base.hs:377:3
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/libposix/posix005.run/posix005.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/getEnvironment02.run/getEnvironment02.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/getEnvironment01.run/getEnvironment01.o_ghc_stgapp"
      -- ext-stg-interpreter: lib/Stg/Interpreter.hs:(215,3)-(230,36): Non-exhaustive patterns in case
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/foldableArray.run/foldableArray.o_ghc_stgapp"
      -- Prelude.chr: bad argument: 671088640
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hSetEncoding001.run/hSetEncoding001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/encoding002.run/encoding002.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/process/tests/T8343.run/T8343.o_ghc_stgapp"
  ]

skip_fail :: [FilePath]
skip_fail =
  [ "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun060.run/cgrun060.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun069.run/cgrun069.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/CgStaticPointersNoFullLazyness.run/CgStaticPointersNoFullLazyness.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/PopCnt.run/PopCnt.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T12622.run/T12622.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T3677.run/T3677.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T5149.run/T5149.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/allocLimit1.run/allocLimit1.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/allocLimit2.run/allocLimit2.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/allocLimit3.run/allocLimit3.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/allocLimit4.run/allocLimit4.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/async001.run/async001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc008.run/conc008.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc009.run/conc009.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc010.run/conc010.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc013.run/conc013.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc014.run/conc014.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc015a.run/conc015a.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc015.run/conc015.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc017a.run/conc017a.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc017.run/conc017.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc018.run/conc018.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc019.run/conc019.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc020.run/conc020.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc021.run/conc021.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc022.run/conc022.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc024.run/conc024.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc029.run/conc029.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc031.run/conc031.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc032.run/conc032.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc033.run/conc033.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc035.run/conc035.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc039.run/conc039.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc040.run/conc040.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc041.run/conc041.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc042.run/conc042.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc043.run/conc043.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc044.run/conc044.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc045.run/conc045.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc058.run/conc058.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc064.run/conc064.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc068.run/conc068.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc071.run/conc071.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/mask001.run/mask001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/mask002.run/mask002.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T4030.run/T4030.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T5238.run/T5238.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T5611a.run/T5611a.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T5611.run/T5611.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T5866.run/T5866.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T7970.run/T7970.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T9379.run/T9379.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/driver/T5313.run/T5313.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/Concurrent/ThreadDelay001.run/ThreadDelay001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/genericNegative001.run/genericNegative001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/countReaders001.run/countReaders001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hGetBuf001.run/hGetBuf001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/openFile005.run/openFile005.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/openFile007.run/openFile007.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/readFile001.run/readFile001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/qsem001.run/qsem001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/qsemn001.run/qsemn001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T10149.run/T10149.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T13167.run/T13167.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T13525.run/T13525.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T15349.run/T15349.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/signals002.run/signals002.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/parser/should_run/CountParserDeps.run/CountParserDeps.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T3245.run/T3245.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/space_leaks/T4018.run/T4018.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T3437.run/T3437.o_ghc_stgapp"
  ]

-- timeout
skip_timeout :: [FilePath]
skip_timeout =
  [ "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/array/should_run/arr017.run/arr017.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun058.run/cgrun058.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/compareByteArrays.run/compareByteArrays.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T15892.run/T15892.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T16846.run/T16846.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/allowinterrupt001.run/allowinterrupt001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/AtomicPrimops.run/AtomicPrimops.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc004.run/conc004.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc007.run/conc007.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc012.run/conc012.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc034.run/conc034.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc051.run/conc051.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc065.run/conc065.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc066.run/conc066.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc067.run/conc067.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/readMVar1.run/readMVar1.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T3279.run/T3279.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T3429.run/T3429.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T367_letnoescape.run/T367_letnoescape.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/throwto003.run/throwto003.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ffi/should_run/ffi020.run/ffi020.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ffi/should_run/fptr02.run/fptr02.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/Concurrent/Chan002.run/Chan002.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/Concurrent/Chan003.run/Chan003.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/CPUTime001.run/CPUTime001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/dynamic003.run/dynamic003.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/dynamic005.run/dynamic005.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/inits.run/inits.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/encoding001.run/encoding001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/encoding004.run/encoding004.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/isEOF001.run/isEOF001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/openFile008.run/openFile008.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/ioref001.run/ioref001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/length001.run/length001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/list003.run/list003.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/memo001.run/memo001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/stableptr001.run/stableptr001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T13191.run/T13191.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T17499.run/T17499.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T3474.run/T3474.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T7653.run/T7653.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T9532.run/T9532.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/T8108.run/T8108.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/should_run/CarryOverflow.run/CarryOverflow.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/should_run/T8726.run/T8726.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join002.run/join002.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join003.run/join003.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join004.run/join004.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join007.run/join007.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/Conversions.run/Conversions.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/InlineArrayAlloc.run/InlineArrayAlloc.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/InlineByteArrayAlloc.run/InlineByteArrayAlloc.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/InlineCloneArrayAlloc.run/InlineCloneArrayAlloc.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/lazy-bs-alloc.run/lazy-bs-alloc.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/MethSharing.run/MethSharing.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T12791.run/T12791.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T14936.run/T14936.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T15226a.run/T15226a.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T15226.run/T15226.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T15426.run/T15426.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T15578.run/T15578.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T3586.run/T3586.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T4474a.run/T4474a.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T4474b.run/T4474b.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T4474c.run/T4474c.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T4978.run/T4978.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T5113.run/T5113.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T5237.run/T5237.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T5536.run/T5536.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T5549.run/T5549.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T5835.run/T5835.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T7257.run/T7257.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T7507.run/T7507.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T7797.run/T7797.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T7850.run/T7850.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T7954.run/T7954.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T8763.run/T8763.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T876.run/T876.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T9203.run/T9203.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/T9339.run/T9339.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/should_run/UniqLoop.run/UniqLoop.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/space_leaks/T2762.run/T2762.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/space_leaks/T4334.run/T4334.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/ArithInt8.run/ArithInt8.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/ArithWord8.run/ArithWord8.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/CmpWord8.run/CmpWord8.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/T10678.run/T10678.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/10queens/10queens.run/10queens.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/andy_cherry/andy_cherry.run/andy_cherry.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/barton-mangler-bug/barton-mangler-bug.run/barton-mangler-bug.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/galois_raytrace/galois_raytrace.run/galois_raytrace.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/jl_defaults/jl_defaults.run/jl_defaults.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/life_space_leak/life_space_leak.run/life_space_leak.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/seward-space-leak/seward-space-leak.run/seward-space-leak.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/thurston-modular-arith/thurston-modular-arith.run/thurston-modular-arith.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quasiquotation/T7918.run/T7918.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/numa001.run/numa001.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/return_mem_to_os.run/return_mem_to_os.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/T2047.run/T2047.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/T2783.run/T2783.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/T7919.run/T7919.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/simplrun004.run/simplrun004.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T5920.run/T5920.o_ghc_stgapp"
  , "/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T5997.run/T5997.o_ghc_stgapp"
  ]
