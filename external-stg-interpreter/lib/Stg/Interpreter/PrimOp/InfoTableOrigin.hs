module Stg.Interpreter.PrimOp.InfoTableOrigin where

import           Control.Applicative  (Applicative (..))

import           Data.Maybe           (Maybe)

import           Foreign.Ptr          (nullPtr)

import           Stg.Interpreter.Base (Atom (..), M, PrimOpEval, PtrOrigin (..))
import           Stg.Syntax           (Name, TyCon, Type)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- whereFrom# :: a -> State# s -> (# State# s, Addr# #)
  ( "whereFrom#", [_a, _s]) -> pure [PtrAtom InfoTablePtr nullPtr]

  _                         -> fallback op args t tc
