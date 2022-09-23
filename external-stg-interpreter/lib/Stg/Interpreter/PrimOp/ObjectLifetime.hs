{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.ObjectLifetime where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- keepAlive# :: v -> State# RealWorld -> (State# RealWorld -> p) -> p
  ( "keepAlive#", [managedObject, s, ioAction@HeapPtr{}]) -> do
    stackPush $ KeepAlive managedObject
    stackPush $ Apply [s]
    pure [ioAction]

  _ -> fallback op args t tc
