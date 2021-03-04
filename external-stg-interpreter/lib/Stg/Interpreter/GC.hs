{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, FlexibleInstances #-}
module Stg.Interpreter.GC where

import Control.Monad.State
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Stg.Interpreter.Base
import Stg.Interpreter.GC.LiveDataAnalysis

{-
TODO:
  done - design GC datalog input/output relations
  done - GC datalog rules
  done - export facts from StgState
-}

runGC :: M ()
runGC = do
  stgState <- get
  LiveData{..} <- liftIO $ runLiveDataAnalysis stgState
  -- TODO: prune stg state
  pure ()
