{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import System.Environment
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as BS8

import Stg.IO
import Stg.GHC.ToStg
import Stg.GHC.Backend

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
main = do

  fullpakPath : mods <- getArgs

  let objectOutputPath = fullpakPath -<.> ".o"
      cg = NCG

  runGhc (Just libdir) $ do
    forM_ mods $ \modName -> do
      let modStgbinName = modName </> modpakStgbinPath
      stgMod <- liftIO $ do
        putStrLn $ "  " ++ modName
        readModpakL fullpakPath modStgbinName decodeStgbin

      let StgModule{..} = toStg stgMod
          oName         = objectOutputPath </> modName ++ ".o"

      -- HINT: the stubs are compiled at link time
      compileToObjectM cg stgUnitId stgModuleName GHC.NoStubs stgModuleTyCons stgTopBindings oName
