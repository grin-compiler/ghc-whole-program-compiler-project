{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Atom where

import qualified Data.Map as Map
import GHC.Stack

import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Allocator

-- atom operations

storeNewAtom :: M sig m => Atom -> m AtomAddr
storeNewAtom a = do
  addr <- freshAtomAddress
  modify $ \s@StgState{..} -> s {ssAtomStore = Map.insert addr a ssAtomStore}
  pure addr

getAtom :: M sig m => AtomAddr -> m Atom
getAtom atomAddr = do
  gets (Map.lookup atomAddr . ssAtomStore) >>= \case
    Nothing   -> stgErrorM $ "missing atom at address: " ++ show atomAddr
    Just atom -> pure atom

getAtoms :: M sig m => [AtomAddr] -> m [Atom]
getAtoms = mapM getAtom

allocAtoms :: M sig m => [Atom] -> m [AtomAddr]
allocAtoms = mapM storeNewAtom
