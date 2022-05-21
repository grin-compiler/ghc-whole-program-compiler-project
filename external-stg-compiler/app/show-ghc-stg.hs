{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import System.Environment

import Stg.IO
import Stg.GHC.ToStg

import qualified GHC.Stg.Syntax       as GHC
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Driver.Session   as GHC

import GHC.Paths ( libdir )
import GHC

showSDoc :: GHC.SDoc -> String
showSDoc = GHC.showSDoc GHC.unsafeGlobalDynFlags

main :: IO ()
main = runGhc (Just libdir) . liftIO $ do

  modpaks <- getArgs
  forM_ modpaks $ \modpakName -> do
    putStrLn $ "reading   " ++ modpakName
    extStgModule <- readModpakL modpakName modpakStgbinPath decodeStgbin
    let StgModule{..} = toStg extStgModule
    putStrLn . showSDoc $ GHC.pprStgTopBindings GHC.panicStgPprOpts stgTopBindings
