{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Stg.Interpreter.StateOp.Env where

import GHC.Stack
import qualified Data.Map as Map
import Data.List (foldl')

import Stg.Syntax
import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Atom

addBinderToEnv :: StaticOrigin -> Binder -> AtomAddr -> Env -> Env
addBinderToEnv so b a = Map.insert (Id b) (so, a)

addZippedBindersToEnv :: StaticOrigin -> [(Binder, AtomAddr)] -> Env -> Env
addZippedBindersToEnv so bvList env = foldl' (\e (b, v) -> Map.insert (Id b) (so, v) e) env bvList

addManyBindersToEnv :: StaticOrigin -> [Binder] -> [AtomAddr] -> Env -> Env
addManyBindersToEnv so binders values = addZippedBindersToEnv so $ zip binders values

lookupEnvSO :: (HasCallStack, M sig m) => Env -> Binder -> m (StaticOrigin, AtomAddr)
lookupEnvSO localEnv b = do
  env <- if binderTopLevel b
          then gets ssStaticGlobalEnv
          else pure localEnv
  case Map.lookup (Id b) env of
    Just a  -> pure a
    Nothing -> case binderUniqueName b of
      -- HINT: GHC.Prim module does not exist it's a wired in module
      "ghc-prim_GHC.Prim.void#"           -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.realWorld#"      -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.coercionToken#"  -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.proxy#"          -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.(##)"            -> (SO_Builtin,) <$> storeNewAtom Void
      _ -> stgErrorM $ "unknown variable: " ++ show b

lookupEnv :: (HasCallStack, M sig m) => Env -> Binder -> m AtomAddr
lookupEnv localEnv b = snd <$> lookupEnvSO localEnv b
