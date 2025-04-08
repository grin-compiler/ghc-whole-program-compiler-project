module Stg.Interpreter.PrimOp.Char where

import           Control.Applicative  (Applicative (..))

import           Data.Char            (ord)
import           Data.Eq              (Eq (..))
import           Data.Function        (($))
import           Data.Maybe           (Maybe)
import           Data.Ord             (Ord (..))

import           Stg.Interpreter.Base
import           Stg.Syntax           (Name, TyCon, Type)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- gtChar# :: Char# -> Char# -> Int#
  ( "gtChar#", [CharV a, CharV b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- geChar# :: Char# -> Char# -> Int#
  ( "geChar#", [CharV a, CharV b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- eqChar# :: Char# -> Char# -> Int#
  ( "eqChar#", [CharV a, CharV b]) -> pure [IntV $ if a == b then 1 else 0]

  -- neChar# :: Char# -> Char# -> Int#
  ( "neChar#", [CharV a, CharV b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- ltChar# :: Char# -> Char# -> Int#
  ( "ltChar#", [CharV a, CharV b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- leChar# :: Char# -> Char# -> Int#
  ( "leChar#", [CharV a, CharV b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ord# :: Char# -> Int#
  ( "ord#",    [CharV c])          -> pure [IntV $ ord c]

  _                                -> fallback op args t tc
