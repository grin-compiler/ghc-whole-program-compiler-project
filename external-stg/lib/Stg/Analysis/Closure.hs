{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Analysis.Closure (getAllClosures) where

import Stg.Syntax

getAllClosures :: Module -> [(Id, Rhs)]
getAllClosures = visitModule

visitAlt :: Alt -> [(Id, Rhs)]
visitAlt Alt{..} = visitExpr altRHS

visitModule :: Module -> [(Id, Rhs)]
visitModule Module{..} = concatMap visitTopBinding moduleTopBindings

visitTopBinding :: TopBinding -> [(Id, Rhs)]
visitTopBinding = \case
  StgTopStringLit{} -> []
  StgTopLifted b    -> visitBinding b

visitBinding :: Binding -> [(Id, Rhs)]
visitBinding = \case
  StgNonRec b rhs -> visitRhs b rhs
  StgRec l        -> concatMap (uncurry visitRhs) l

visitExpr :: Expr -> [(Id, Rhs)]
visitExpr = \case
  StgLet b expr         -> visitBinding b ++ visitExpr expr
  StgLetNoEscape b expr -> visitBinding b ++ visitExpr expr
  StgCase expr _ _ alts -> visitExpr expr ++ concatMap visitAlt alts
  StgTick _ expr        -> visitExpr expr
  _                     -> []

visitRhs :: Binder -> Rhs -> [(Id, Rhs)]
visitRhs b rhs = case rhs of
  StgRhsClosure _ _ _ expr  -> (Id b, rhs) : visitExpr expr
  _                         -> []
