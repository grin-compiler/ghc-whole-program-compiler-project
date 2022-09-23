{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Unsafe where

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of
  -- reallyUnsafePtrEquality# :: a -> b -> Int#
  ( "reallyUnsafePtrEquality#", [a, b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Unsafe pointer equality"
--  (#1 Bad Guy: Alastair Reid :)
------------------------------------------------------------------------

primop  ReallyUnsafePtrEqualityOp "reallyUnsafePtrEquality#" GenPrimOp
   a -> a -> Int#
   { Returns {\texttt 1\#} if the given pointers are equal and {\texttt 0\#} otherwise. }
   with
   can_fail   = True -- See Note [reallyUnsafePtrEquality#]


-- Note [reallyUnsafePtrEquality#]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- reallyUnsafePtrEquality# can't actually fail, per se, but we mark it can_fail
-- anyway. Until 5a9a1738023a, GHC considered primops okay for speculation only
-- when their arguments were known to be forced. This was unnecessarily
-- conservative, but it prevented reallyUnsafePtrEquality# from floating out of
-- places where its arguments were known to be forced. Unfortunately, GHC could
-- sometimes lose track of whether those arguments were forced, leading to let/app
-- invariant failures (see #13027 and the discussion in #11444). Now that
-- ok_for_speculation skips over lifted arguments, we need to explicitly prevent
-- reallyUnsafePtrEquality# from floating out. Imagine if we had
--
--     \x y . case x of x'
--              DEFAULT ->
--            case y of y'
--              DEFAULT ->
--               let eq = reallyUnsafePtrEquality# x' y'
--               in ...
--
-- If the let floats out, we'll get
--
--     \x y . let eq = reallyUnsafePtrEquality# x y
--            in case x of ...
--
-- The trouble is that pointer equality between thunks is very different
-- from pointer equality between the values those thunks reduce to, and the latter
-- is typically much more precise.
-}