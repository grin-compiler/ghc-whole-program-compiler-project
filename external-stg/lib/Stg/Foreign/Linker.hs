{-# LANGUAGE RecordWildCards #-}
module Stg.Foreign.Linker where

import Data.List
import System.Directory
import System.FilePath
import System.Process
import Text.Printf

import Stg.Program
import Stg.Syntax
import Stg.Foreign.Stubs

getExtStgWorkDirectory :: FilePath -> IO FilePath
getExtStgWorkDirectory ghcstgappFname = do
  absFname <- makeAbsolute ghcstgappFname
  pure $ takeDirectory absFname </> ".ext-stg-work" </> takeBaseName ghcstgappFname

linkForeignCbitsSharedLib :: FilePath -> IO ()
linkForeignCbitsSharedLib ghcstgappFname = do

  workDir <- getExtStgWorkDirectory ghcstgappFname
  createDirectoryIfMissing True workDir

  let stubFname = workDir </> "stub.c"
  genStubs ghcstgappFname >>= writeFile stubFname

  (StgAppLinkerInfo{..}, linkerInfoList') <- getAppLinkerInfo ghcstgappFname
  let linkerInfoList = filter (\StgLibLinkerInfo{..} -> not (isPrefixOf "rts-1" stglibName)) linkerInfoList'

      cbitsOpts =
          [ unwords $ concat
            [ [ "-L" ++ dir | dir <- stglibExtraLibDirs]
            , stglibLdOptions
            , [ "-l" ++ lib | lib <- stglibExtraLibs]
            ]
          | StgLibLinkerInfo{..} <- linkerInfoList
          ]

      cbitsArs = concatMap stglibCbitsPaths linkerInfoList

      stubArs = concatMap stglibStubsPaths linkerInfoList

      arList = case cbitsArs ++ stubArs of
          []  -> []
          l   -> ["-Wl,--whole-archive"] ++ l ++ ["-Wl,--no-whole-archive"]

      stubOpts =
          [ "-fPIC"
          , "stub.c"
          ]

      appOpts = unwords $ concat
        [ [ "-L" ++ dir | dir <- stgappExtraLibDirs]
        , stgappLdOptions
        , [ "-l" ++ lib | lib <- stgappExtraLibs]
        ]

      linkScript =
        unlines
          [ "#!/usr/bin/env bash"
          , "set -e"
          , "gcc -o cbits.so -shared \\"
          , "  -Wl,--no-as-needed \\"

          -- TODO: remove this
          , "  `ls /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/data/cbits.so-script/rts.dyn_o/*` \\"

          ] ++
        intercalate " \\\n" (map ("  " ++) . filter (/= "") $ arList ++ cbitsOpts ++ stgappCObjects ++ [appOpts] ++ stubOpts) ++ "\n"

  let scriptFname = workDir </> "cbits.so.sh"
  writeFile scriptFname linkScript
  callCommand $ printf "(cd %s; bash cbits.so.sh)" workDir
