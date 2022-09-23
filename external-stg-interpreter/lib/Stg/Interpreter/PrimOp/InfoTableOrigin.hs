{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.InfoTableOrigin where

import Foreign.Ptr

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- whereFrom# :: a -> State# s -> (# State# s, Addr# #)
  ( "whereFrom#", [_a, _s]) -> pure [PtrAtom InfoTablePtr nullPtr]

  _ -> fallback op args t tc
