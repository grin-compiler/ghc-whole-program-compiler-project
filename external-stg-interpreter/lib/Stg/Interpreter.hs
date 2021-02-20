{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Stg.Interpreter where

import GHC.Stack
import qualified GHC.Exts as Exts
import Foreign.Ptr

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Control.Exception
import qualified Data.Primitive.ByteArray as BA

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import System.Posix.DynamicLinker

import System.Environment (getArgs)
import System.FilePath (takeExtension, takeBaseName, dropExtension, takeDirectory)
import System.IO
import System.Directory

import Stg.Syntax
import Stg.Program

import Stg.Interpreter.Base
import Stg.Interpreter.PrimCall
import Stg.Interpreter.FFI
import Stg.Interpreter.Rts
import Stg.Interpreter.Debug
import qualified Stg.Interpreter.ThreadScheduler as Scheduler

import qualified Stg.Interpreter.PrimOp.Addr          as PrimAddr
import qualified Stg.Interpreter.PrimOp.Array         as PrimArray
import qualified Stg.Interpreter.PrimOp.SmallArray    as PrimSmallArray
import qualified Stg.Interpreter.PrimOp.ArrayArray    as PrimArrayArray
import qualified Stg.Interpreter.PrimOp.ByteArray     as PrimByteArray
import qualified Stg.Interpreter.PrimOp.Char          as PrimChar
import qualified Stg.Interpreter.PrimOp.Concurrency   as PrimConcurrency
import qualified Stg.Interpreter.PrimOp.DelayWait     as PrimDelayWait
import qualified Stg.Interpreter.PrimOp.Parallelism   as PrimParallelism
import qualified Stg.Interpreter.PrimOp.Exceptions    as PrimExceptions
import qualified Stg.Interpreter.PrimOp.Float         as PrimFloat
import qualified Stg.Interpreter.PrimOp.Double        as PrimDouble
import qualified Stg.Interpreter.PrimOp.Word          as PrimWord
import qualified Stg.Interpreter.PrimOp.Word8         as PrimWord8
import qualified Stg.Interpreter.PrimOp.Word16        as PrimWord16
import qualified Stg.Interpreter.PrimOp.Int           as PrimInt
import qualified Stg.Interpreter.PrimOp.Int8          as PrimInt8
import qualified Stg.Interpreter.PrimOp.Int16         as PrimInt16
import qualified Stg.Interpreter.PrimOp.MutVar        as PrimMutVar
import qualified Stg.Interpreter.PrimOp.MVar          as PrimMVar
import qualified Stg.Interpreter.PrimOp.Narrowings    as PrimNarrowings
import qualified Stg.Interpreter.PrimOp.Prefetch      as PrimPrefetch
import qualified Stg.Interpreter.PrimOp.StablePointer as PrimStablePointer
import qualified Stg.Interpreter.PrimOp.WeakPointer   as PrimWeakPointer
import qualified Stg.Interpreter.PrimOp.TagToEnum     as PrimTagToEnum
import qualified Stg.Interpreter.PrimOp.Unsafe        as PrimUnsafe
import qualified Stg.Interpreter.PrimOp.MiscEtc       as PrimMiscEtc

{-
  Q: what is the operational semantic of StgApp
  A: check slowCall :: CmmExpr -> [StgArg] -> FCode ReturnKind semantics
  done - read STG eval apply paper

  TODO:
    - refresh all STG uniques to be globally unique
    - collect free variables for closures

  EVAL:
    - setup top level env (TopEnv) ; add top level bindings to it
    - call main
-}

{-
data LitNumType
  = LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Eq, Ord, Generic, Show)

data Lit
  = LitChar     !Char
  | LitString   !BS.ByteString
  | LitNumber   !LitNumType !Integer
-}

evalLiteral :: HasCallStack => Lit -> M Atom
evalLiteral = \case
  LitString str -> getCStringConstantPtrAtom str
  LitFloat f    -> pure . FloatAtom $ realToFrac f
  LitDouble d   -> pure . DoubleAtom $ realToFrac d
  LitNullAddr   -> pure $ PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> pure . WordAtom $ fromIntegral n
  l -> pure $ Literal l

evalArg :: HasCallStack => Env -> Arg -> M Atom
evalArg localEnv = \case
  StgLitArg l -> evalLiteral l
  StgVarArg b -> lookupEnv localEnv b

{-
  TODO: support these
  | ApStack                   -- HINT: needed for the async exceptions
    { hoResult      :: [Atom]
    , hoStack       :: [StackContinuation]
    }
-}

builtinStgEval :: HasCallStack => Atom -> M [Atom]
builtinStgEval a@HeapPtr{} = do
  o <- readHeap a
  case o of
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}       -> pure [a]
    BlackHole t -> stgErrorM $ "blackhole ; loop in evaluation of : " ++ show t
    Closure{..}
      | hoCloMissing /= 0
      -> pure [a]

      | otherwise
      -> do
        --liftIO $ print hoName
        modify' $ \s -> s {ssCurrentClosure = hoName}
        let StgRhsClosure uf params e = hoCloBody
            HeapPtr l = a
            extendedEnv = addManyBindersToEnv params hoCloArgs hoEnv
        markExecuted l
        -- TODO: env or free var handling
        case uf of
          ReEntrant -> do
            --liftIO $ putStrLn " * eval - ReEntrant"
            -- closure may be entered multiple times, but should not be updated or blackholed.
            evalExpr extendedEnv e
          Updatable -> do
            --liftIO $ putStrLn " * eval - Updatable"
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            stackPush (Update l)
            store l (BlackHole o)
            evalExpr extendedEnv e
          SingleEntry -> do
            --liftIO $ putStrLn " * eval - SingleEntry"
            -- closure will only be entered once, and so need not be updated but may safely be blackholed.
            store l (BlackHole o)
            evalExpr extendedEnv e
    _ -> stgErrorM $ "expected heap object: " ++ show o
--builtinStgEval x = pure [x] -- FIXME: this is a debug hack!! remove it!!!!!
builtinStgEval a = stgErrorM $ "expected a thunk, got: " ++ show a

builtinStgApply :: HasCallStack => Atom -> [Atom] -> M [Atom]
builtinStgApply a [] = builtinStgEval a
builtinStgApply a@HeapPtr{} args = do
  let argCount      = length args
      HeapPtr addr  = a
  o <- readHeap a
  case o of
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}             -> stgErrorM $ "unexpexted con at apply: "-- ++ show o
    BlackHole t       -> stgErrorM $ "blackhole ; loop in application of : " ++ show t
    Closure{..}
      -- under saturation
      | hoCloMissing - argCount > 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        pure [HeapPtr newAp]

      -- over saturation
      | hoCloMissing - argCount < 0
      -> do
        let (satArgs, remArgs) = splitAt hoCloMissing args
        stackPush (Apply remArgs)
        builtinStgApply a satArgs

      -- saturation
      | hoCloMissing - argCount == 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        builtinStgEval (HeapPtr newAp)

builtinStgApply a args = stgErrorM $ "builtinStgApply - expected a closure (ptr), got: " ++ show a ++ ", args: " ++ show args
{-
heapObjectKind :: HeapObject -> String
heapObjectKind = \case
  Con{}             -> "Con"
  Closure{}         -> "Closure"
  BlackHole{}       -> "BlackHole"
  RaiseException{}  -> "RaiseException"
  ApStack{}         -> "ApStack"

peekResult :: [Atom] -> M String
peekResult [hp@HeapPtr{}] = do
  o <- readHeap hp
  case o of
    Con dc args -> pure $ "Con: " ++ show (dataConUniqueName dc) ++ " " ++ show args
    Closure{..} -> pure $ "Closure missing: " ++ show hoCloMissing ++ " args: " ++ show hoCloArgs
    BlackHole{} -> pure "BlackHole"
peekResult r = pure $ show r
-}
assertWHNF :: HasCallStack => [Atom] -> AltType -> Binder -> M ()
assertWHNF [hp@HeapPtr{}] aty res = do
  o <- readHeap hp
  case o of
    Con dc args -> pure ()
    Closure{..}
      | hoCloMissing == 0
      , aty /= MultiValAlt 1
      -> do
          liftIO $ do
            putStrLn "Thunk"
            putStrLn ""
            print aty
            putStrLn ""
            print res
            putStrLn ""
          error "Thunk"
      | otherwise         -> pure ()
    BlackHole{} -> error "BlackHole"
assertWHNF _ _ _ = pure ()

{-
  machine state:
    stack
    local env (local bindings only = non-top-level)
    global env (top-level bindings only) (read-only)
    heap

  NOTES:
    closure execution always clears the local env (Apply stack constructor)
    case continuation restores the saved local env (Case stack constructor)
    the update stack constructor does not touch the local environment

  IDEA:
    model the local environment as a parameter of evalExpr
-}

{-
evalOnMainThread  :: M [Atom] -> M [Atom] -- setup_action_with_initial_result -> result
evalOnNewThread   :: M [Atom] -> M [Atom] -- setup_action_with_initial_result -> result

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled
-}

isThreadLive :: ThreadStatus -> Bool
isThreadLive = \case
  ThreadFinished  -> False
  ThreadDied      -> False
  _ -> True

killAllThreads :: M ()
killAllThreads = do
  pure () -- TODO

evalOnMainThread :: M [Atom] -> M [Atom]
evalOnMainThread = evalOnThread True

evalOnNewThread :: M [Atom] -> M [Atom]
evalOnNewThread = evalOnThread False

evalOnThread :: Bool -> M [Atom] -> M [Atom]
evalOnThread isMainThread setupAction = do
  -- create main thread
  (tid, ts) <- createThread
  scheduleToTheEnd tid
  switchToThread tid

  stackPush $ RunScheduler SR_ThreadFinished
  result0 <- setupAction
  let loop resultIn = do
        resultOut <- evalStackMachine resultIn
        --reportThreads
        ThreadState{..} <- getThreadState tid
        case isThreadLive tsStatus of
          True  -> loop resultOut -- HINT: the new scheduling is ready
          False -> do
            when isMainThread killAllThreads
            pure tsCurrentResult
  loop result0

evalStackMachine :: [Atom] -> M [Atom]
evalStackMachine result = do
  tid <- gets ssCurrentThreadId
  --liftIO $ putStrLn "."
  stackPop >>= \case
    Nothing -> do
      --liftIO $ putStrLn $ "evalStackMachine - empty stack, tid: " ++ show tid
      pure result
    Just stackCont  -> evalStackContinuation result stackCont >>= evalStackMachine

evalStackContinuation :: [Atom] -> StackContinuation -> M [Atom]
evalStackContinuation result = \case
  Apply args
    | [fun@HeapPtr{}] <- result
    -> builtinStgApply fun args

  Update dstAddr
    | [src@HeapPtr{}] <- result
    -> do
      o <- readHeap src
      store dstAddr o
      pure result

  -- HINT: STG IR uses 'case' expressions to chain instructions with strict evaluation
  CaseOf curClosure localEnv resultBinder altType alts -> do
    modify' $ \s -> s {ssCurrentClosure = curClosure}
    assertWHNF result altType resultBinder
    case altType of
      AlgAlt tc -> do
        let v = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv resultBinder v localEnv
        con <- readHeapCon v
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstCon extendedEnv con $ al ++ [d]
          _ -> matchFirstCon extendedEnv con alts

      PrimAlt r -> do
        let lit = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv resultBinder lit localEnv
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstLit extendedEnv lit $ al ++ [d]
          _ -> matchFirstLit extendedEnv lit alts

      MultiValAlt _n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [Alt{..}] = alts
            extendedEnv = addManyBindersToEnv altBinders result localEnv
        evalExpr extendedEnv altRHS

      PolyAlt -> do
        let [Alt{..}]   = alts
            [v]         = result
            extendedEnv = addBinderToEnv resultBinder v $                 -- HINT: bind the result
                          addManyBindersToEnv altBinders result localEnv  -- HINT: bind alt params
        evalExpr extendedEnv altRHS

  RestoreExMask b i -> do
    -- TODO
    pure result

  Catch h b i -> do
    -- TODO: is anything to do??
    pure result

  RunScheduler sr -> Scheduler.runScheduler result sr

  -- HINT: dataToTag# has an eval call in the middle, that's why we need this continuation, it is the post-returning part of the op implementation
  DataToTagOp -> PrimTagToEnum.dataToTagOp result

  x -> error $ "unsupported continuation: " ++ show x ++ ", result: " ++ show result

evalExpr :: HasCallStack => Env -> Expr -> M [Atom]
evalExpr localEnv = \case
  StgTick _ e       -> evalExpr localEnv e
  StgLit l          -> pure <$> evalLiteral l
  StgConApp dc l _
    -- HINT: make and return unboxed tuple
    | UnboxedTupleCon{} <- dcRep dc
    -> mapM (evalArg localEnv) l   -- Q: is this only for unboxed tuple? could datacon be heap allocated?

    -- HINT: create boxed datacon on the heap
    | otherwise
    -> do
      args <- mapM (evalArg localEnv) l
      loc <- allocAndStore (Con dc args)
      pure [HeapPtr loc]

  StgLet b e -> do
    --liftIO $ putStrLn " * StgLet"
    extendedEnv <- declareBinding localEnv b
    evalExpr extendedEnv e

  StgLetNoEscape b e -> do -- TODO: do not allocate closure on heap, instead put into env (stack) allocated closure ; model stack allocated heap objects
    --liftIO $ putStrLn " * StgLetNoEscape"
    extendedEnv <- declareBinding localEnv b
    evalExpr extendedEnv e

  -- var (join id)
  StgApp i [] _t _
    | JoinId 0 <- binderDetails i
    -> do
      --liftIO $ putStrLn " * StgApp var (join id)"
      -- HINT: join id-s are always closures, needs eval
      -- NOTE: join id's type tells the closure return value representation
      v <- lookupEnv localEnv i
      builtinStgEval v

    | JoinId x <- binderDetails i
    -> stgErrorM $ "join-id var arity error, expected 0, got: " ++ show x ++ " id: " ++ show i

  -- var (non join id)
  StgApp i [] _t _ -> case binderType i of

    SingleValue LiftedRep -> do
      --liftIO $ putStrLn $ " * StgApp var (non join id): " ++ show (binderType i)
      -- HINT: must be HeapPtr ; read heap ; check if Con or Closure ; eval if Closure ; return HeapPtr if Con
      v <- lookupEnv localEnv i
      --liftIO $ putStrLn $ " * 1"
      --liftIO $ putStrLn $ "force: " ++ show (Id i) ++ " " ++ show v
      result <- builtinStgEval v
      --liftIO $ putStrLn $ " * 2"
      --pResult <- peekResult result
      --liftIO $ putStrLn $ "force-result: " ++ show (Id i) ++ " = " ++ show result ++ " " ++ pResult
      pure result

    SingleValue _ -> do
      --liftIO $ putStrLn $ " * StgApp var (non join id): " ++ show (binderType i)
      --liftIO $ putStrLn $ " * 3"
      v <- lookupEnv localEnv i
      --liftIO $ putStrLn $ " * 4"
      pure [v]

    UnboxedTuple []
      | binderId i == BinderId (Unique '0' 124) -- wired in coercion token
      -> do
        pure []

    r -> stgErrorM $ "unsupported var rep: " ++ show r ++ " " ++ show i -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  --  A: the join id type is for the return value representation and not for the id representation, so it can be unlifted.
  StgApp i l _t _
    | JoinId _ <- binderDetails i
    -> do
      --liftIO $ putStrLn " * StgApp fun app join id"
      args <- mapM (evalArg localEnv) l
      v <- lookupEnv localEnv i
      builtinStgApply v args

  {- non-join id -}
  StgApp i l _t _ -> case binderType i of
    SingleValue LiftedRep -> do
      --liftIO $ putStrLn " * StgApp fun app non-join id"
      args <- mapM (evalArg localEnv) l
      v <- lookupEnv localEnv i
      --liftIO $ putStrLn $ "call: " ++ show (Id i) ++ " " ++ show args 
      result <- builtinStgApply v args
      --pResult <- peekResult result
      --liftIO $ putStrLn $ "call-result: " ++ show (Id i) ++ " " ++ show args ++ " = " ++ show result ++ " " ++ pResult
      pure result

    r -> stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e scrutineeResult altType alts -> do
    --liftIO $ putStrLn " * StgCase"
    curClosure <- gets ssCurrentClosure
    stackPush (CaseOf curClosure localEnv scrutineeResult altType alts)
    evalExpr localEnv e

  StgOpApp (StgPrimOp op) l t tc -> do
    args <- mapM (evalArg localEnv) l
    evalStack <- gets ssEvalStack
    tid <- gets ssCurrentThreadId
    --liftIO $ putStrLn $ show tid ++ "  " ++ show op ++ " " ++ show args ++ " = ..."
    result <- evalPrimOp op args t tc
    --liftIO $ putStrLn $ show (head evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = " ++ show result
    markPrimOp op
    pure result

  StgOpApp (StgFCallOp foreignCall) l t tc -> do
    args <- mapM (evalArg localEnv) l
    result <- evalFCallOp evalOnNewThread foreignCall args t tc
    evalStack <- gets ssEvalStack
    --liftIO $ putStrLn $ show (head evalStack) ++ " " ++ show foreignCall ++ " " ++ show args ++ " = " ++ show result
    --liftIO $ putStrLn $ show foreignCall ++ " " ++ show args ++ " = " ++ show result
    markFFI foreignCall
    pure result
{-
  StgOpApp (StgPrimCallOp (PrimCall "stg_getThreadAllocationCounterzh" _)) _args t _tc -> do
    i <- gets ssNextAddr
    pure [IntAtom (-i)]
-}
  StgOpApp (StgPrimCallOp primCall) l t tc -> do
    args <- mapM (evalArg localEnv) l
    result <- evalPrimCallOp primCall args t tc
    markPrimCall primCall
    pure result

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t


matchFirstLit :: HasCallStack => Env -> Atom -> [Alt] -> M [Atom]
matchFirstLit localEnv a ([Alt AltDefault _ rhs]) = evalExpr localEnv rhs
matchFirstLit localEnv atom alts = case head $ [a | a@Alt{..} <- alts, matchLit atom altCon] ++ (error $ "no lit match" ++ show (atom, map altCon alts)) of
  Alt{..} -> do
    evalExpr localEnv altRHS
matchFirstLit localEnv l alts = error $ "no lit match" ++ show (localEnv, l, map altCon alts)

matchLit :: HasCallStack => Atom -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  -> False
  AltLit l      -> a == convertAltLit l
  AltDefault    -> True

convertAltLit :: Lit -> Atom
convertAltLit = \case
  LitFloat f                -> FloatAtom $ realToFrac f
  LitDouble d               -> DoubleAtom $ realToFrac d
  LitNullAddr               -> PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> WordAtom $ fromIntegral n
  l -> Literal l


matchFirstCon :: HasCallStack => Env -> HeapObject -> [Alt] -> M [Atom]
matchFirstCon localEnv (Con dc args) alts = case head $ [a | a@Alt{..} <- alts, matchCon dc altCon] ++ error "no matching alts" of
  Alt{..} -> do
    let extendedEnv = addManyBindersToEnv altBinders args localEnv
    evalExpr extendedEnv altRHS

matchCon :: HasCallStack => DataCon -> AltCon -> Bool
matchCon a = \case
  AltDataCon dc -> dataConUniqueName a == dataConUniqueName dc
  AltLit{}      -> False
  AltDefault    -> True

declareBinding :: HasCallStack => Env -> Binding -> M Env
declareBinding localEnv = \case
  StgNonRec b rhs -> do
    addr <- freshHeapAddress
    storeRhs localEnv b addr rhs
    pure $ addBinderToEnv b (HeapPtr addr) localEnv

  StgRec l -> do
    (ls, newEnvItems) <- fmap unzip . forM l $ \(b, _) -> do
      addr <- freshHeapAddress
      pure (addr, (b, (HeapPtr addr)))
    let extendedEnv = addZippedBindersToEnv newEnvItems localEnv
    forM_ (zip ls l) $ \(addr, (b, rhs)) -> do
      storeRhs extendedEnv b addr rhs
    pure extendedEnv

storeRhs :: HasCallStack => Env -> Binder -> Addr -> Rhs -> M ()
storeRhs localEnv i addr = \case
  StgRhsCon dc l -> do
    args <- mapM (evalArg localEnv) l
    store addr (Con dc args)

  cl@(StgRhsClosure _ paramNames _) -> do
    let prunedEnv = localEnv -- TODO: do pruning to keep only the live/later referred variables
    store addr (Closure (Id i) cl prunedEnv [] (length paramNames))

-----------------------

declareTopBindings :: HasCallStack => [Module] -> M ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ (concatMap moduleTopBindings) mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  stringEnv <- forM strings $ \(StgTopStringLit b str) -> do
    strPtr <- getCStringConstantPtrAtom str
    pure (Id b, strPtr)

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l

  (closureEnv, rhsList) <- fmap unzip . forM bindings $ \(b, rhs) -> do
    addr <- freshHeapAddress
    pure ((Id b, HeapPtr addr), (b, addr, rhs))

  -- set the top level binder env
  modify' $ \s@StgState{..} -> s {ssStaticGlobalEnv = Map.fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  forM_ rhsList $ \(b, addr, rhs) -> storeRhs mempty b addr rhs
{-
builtinStackMachineApply :: Atom -> [Atom] -> M [Atom]
builtinStackMachineApply fun args = do
  --liftIO $ putStrLn $ "builtinStackMachineApply, fun: " ++ show fun ++ ", args: " ++ show args
  stackPush $ Apply args
  evalStackMachine [fun]
-}
{-
builtinStackMachineEval :: Atom -> M [Atom]
builtinStackMachineEval thunk = do
  --liftIO $ putStrLn $ "builtinStackMachineEval: " ++ show thunk
  builtinStackMachineApply thunk []
-}

test :: HasCallStack => IO ()
test = do
  --mods0 <- getFullpakModules "minigame.fullpak"
  --mods0 <- getFullpakModules "tsumupto.fullpak"
  --mods0 <- getFullpakModules "words.fullpak"
  (switchCWD, fullpak_name, progArgs) <- getArgs >>= \case
    "-cwd" : n : args -> pure (True, n, args)
    n : args -> pure (False, n, args)
    _   -> pure (False, "tsumupto.fullpak", [])

  mods0 <- case takeExtension fullpak_name of
    ".fullpak"          -> getFullpakModules fullpak_name
    ".o_ghc_stgapp"     -> getGhcStgAppModules fullpak_name
    ".dyn_o_ghc_stgapp" -> getGhcStgAppModules fullpak_name
    _ -> error "unknown input file format"

  let mods      = extStgRtsSupportModule : mods0 -- NOTE: add RTS support module
      progName  = dropExtension fullpak_name

  currentDir <- liftIO getCurrentDirectory
  stgappDir <- makeAbsolute $ takeDirectory fullpak_name
  --putStrLn $ "progName: " ++ show progName ++ " progArgs: " ++ show progArgs
  let run = do
        when switchCWD $ liftIO $ setCurrentDirectory stgappDir
        declareTopBindings mods
        initRtsSupport progName progArgs mods
        env <- gets ssStaticGlobalEnv
        --liftIO $ putStrLn $ "top Id count: " ++ show (Map.size env)
        let rootMain = unId $ head $ [i | i <- Map.keys env, show i == "main_:Main.main"]
        limit <- gets ssNextAddr
        modify' $ \s@StgState{..} -> s {ssAddressAfterInit = limit}

        -- TODO: check how it is done in the native RTS: call hs_main
        mainAtom <- lookupEnv mempty rootMain

        evalOnMainThread $ do
          stackPush $ Apply [Void]
          pure [mainAtom]

        {-
        Capability *cap = rts_lock();
        rts_evalLazyIO(&cap, main_closure, NULL);
        rts_unlock(cap);
        -}
        flushStdHandles
        --showDebug evalOnNewThread
        -- TODO: do everything that 'hs_exit_' does

  stateStore <- newEmptyMVar
  dl <- dlopen "./libHSbase-4.14.0.0.cbits.so" [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  flip catch (\e -> do {dlclose dl; throw (e :: SomeException)}) $ do
    s@StgState{..} <- execStateT run (emptyStgState (PrintableMVar stateStore) dl)
    when switchCWD $ setCurrentDirectory currentDir
    dlclose dl

    --putStrLn $ unlines $ [BS8.unpack $ binderUniqueName b | Id b <- Map.keys ssEnv]
    --print ssNextAddr
    --print $ head $ Map.toList ssEnv
    -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main

-------------------------

-------------------------

flushStdHandles :: M ()
flushStdHandles = do
  --liftIO $ putStrLn "\n *** flushStdHandles\n"
  Rts{..} <- gets ssRtsSupport
  evalOnNewThread $ do
    stackPush $ Apply [] -- HINT: force IO monad result to WHNF
    stackPush $ Apply [Void]
    pure [rtsTopHandlerFlushStdHandles]
{-
  (tid, ts) <- createThread
  insertThread tid ts
  scheduleToTheEnd tid
  switchToThread tid

  -- force result to WHNF
  --stackPush $ Apply []
  resultLazy <- builtinStackMachineApply rtsTopHandlerFlushStdHandles [Void]
  case resultLazy of
    []            -> pure resultLazy
    [valueThunk]  -> builtinStgEval valueThunk -- pure resultLazy -- builtinStackMachineApply valueThunk []
-}
  pure ()



{-
// Flush stdout and stderr.  We do this during shutdown so that it
// happens even when the RTS is being used as a library, without a
// main (#5594)
static void flushStdHandles(void)
{
    Capability *cap;
    cap = rts_lock();
    rts_evalIO(&cap, flushStdHandles_closure, NULL);
    rts_unlock(cap);
}

/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
void rts_evalIO (/* inout */ Capability **cap,
                 /* in    */ HaskellObj p,
                 /* out */   HaskellObj *ret)
{
    StgTSO* tso;

    tso = createStrictIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

StgTSO *
createStrictIOThread(Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread(cap, stack_size);
  pushClosure(t, (W_)&stg_forceIO_info);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}
-}

{-
/*
 * Like rts_evalIO(), but doesn't force the action's result.
 */
void rts_evalLazyIO (/* inout */ Capability **cap,
                     /* in    */ HaskellObj p,
                     /* out */   HaskellObj *ret)
{
    StgTSO *tso;

    tso = createIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

StgTSO *
createIOThread (Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread (cap, stack_size);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

void
scheduleWaitThread (StgTSO* tso, /*[out]*/HaskellObj* ret, Capability **pcap)
{
    Task *task;
    DEBUG_ONLY( StgThreadID id );
    Capability *cap;

    cap = *pcap;

    // We already created/initialised the Task
    task = cap->running_task;

    // This TSO is now a bound thread; make the Task and TSO
    // point to each other.
    tso->bound = task->incall;
    tso->cap = cap;

    task->incall->tso = tso;
    task->incall->ret = ret;
    task->incall->rstat = NoStatus;

    appendToRunQueue(cap,tso);

    DEBUG_ONLY( id = tso->id );
    debugTrace(DEBUG_sched, "new bound thread (%lu)", (unsigned long)id);

    cap = schedule(cap,task);

    ASSERT(task->incall->rstat != NoStatus);
    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    debugTrace(DEBUG_sched, "bound thread (%lu) finished", (unsigned long)id);
    *pcap = cap;
}
-}
---------------------- primops

evalPrimOp :: HasCallStack => Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp =
  PrimAddr.evalPrimOp $
  PrimArray.evalPrimOp $
  PrimSmallArray.evalPrimOp $
  PrimArrayArray.evalPrimOp $
  PrimByteArray.evalPrimOp $
  PrimChar.evalPrimOp $
  PrimConcurrency.evalPrimOp $
  PrimDelayWait.evalPrimOp $
  PrimParallelism.evalPrimOp $
  PrimExceptions.evalPrimOp $
  PrimFloat.evalPrimOp $
  PrimDouble.evalPrimOp $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
  PrimMutVar.evalPrimOp $
  PrimMVar.evalPrimOp $
  PrimNarrowings.evalPrimOp $
  PrimPrefetch.evalPrimOp $
  PrimStablePointer.evalPrimOp $
  PrimWeakPointer.evalPrimOp $
  PrimWord16.evalPrimOp $
  PrimWord8.evalPrimOp $
  PrimWord.evalPrimOp $
  PrimTagToEnum.evalPrimOp $
  PrimUnsafe.evalPrimOp $
  PrimMiscEtc.evalPrimOp $
  unsupported where
    unsupported op args _t _tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
