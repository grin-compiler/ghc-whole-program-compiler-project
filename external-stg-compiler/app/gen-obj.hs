{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import System.Environment

import Stg.IO
import Stg.GHC.ToStg
import Stg.GHC.Backend
import Stg.DeadFunctionElimination.StripModule

import qualified GHC.Driver.Types as GHC

import GHC
import GHC.Paths ( libdir )

{-
  = StgModule
  { stgUnitId       :: UnitId
  , stgModuleName   :: ModuleName
  , stgModuleTyCons :: [TyCon]
  , stgTopBindings  :: [StgTopBinding]
  , stgForeignStubs :: ForeignStubs
  , stgForeignFiles :: [(ForeignSrcLang, FilePath)]
  }
-}

main :: IO ()
main = runGhc (Just libdir) $ do
  let cg = NCG

  modpaks <- liftIO getArgs
  forM_ modpaks $ \modpakName -> do
    extStgModule <- liftIO $ do
      putStrLn $ modpakName
      readModpakL modpakName modpakStgbinPath decodeStgbin

    strippedExtModule <- liftIO $ tryStripDeadParts {-modpakName-}"." extStgModule -- TODO: fix liveness input name

    let StgModule{..} = toStg strippedExtModule
        oName         = modpakName ++ ".o"
    --liftIO $ putStrLn $ "compiling " ++ oName
    --putStrLn $ unlines $ map show stgIdUniqueMap

    -- HINT: the stubs are compiled at link time
    compileToObjectM cg stgUnitId stgModuleName GHC.NoStubs stgModuleTyCons stgTopBindings oName

    -- TODO: simplify API to: compileToObject cg stgModule oName
