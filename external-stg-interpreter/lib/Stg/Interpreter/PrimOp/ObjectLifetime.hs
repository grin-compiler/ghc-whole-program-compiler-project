module Stg.Interpreter.PrimOp.ObjectLifetime where

import           Control.Applicative  (Applicative (..))

import           Data.Function        (($))
import           Data.Maybe           (Maybe)

import           Stg.Interpreter.Base (Atom (..), M, PrimOpEval, StackContinuation (..), stackPush)
import           Stg.Syntax           (Name, TyCon, Type)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- keepAlive# :: v -> State# RealWorld -> (State# RealWorld -> p) -> p
  ( "keepAlive#", [managedObject, s, ioAction@HeapPtr{}]) -> do
    stackPush $ KeepAlive managedObject
    stackPush $ Apply [s]
    pure [ioAction]

  _ -> fallback op args t tc
