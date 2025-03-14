{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Analysis.LiveVariable (annotateWithLiveVariables) where

import Data.Set (Set)
import qualified Data.Set as Set

import Stg.Syntax

type UsedLocal = Set Id

annotateWithLiveVariables :: Module -> Module
annotateWithLiveVariables = visitModule

-- HINT: used local bindings
mkUsedLocal :: [Binder] -> UsedLocal
mkUsedLocal l = Set.fromList [Id b | b <- l, not (binderTopLevel b)]

remove :: [Binder] -> UsedLocal -> UsedLocal
remove l u = foldr (Set.delete . Id) u l

visitAlt :: Alt -> (UsedLocal, Alt)
visitAlt a@Alt{..} = (remove altBinders u, a {altRHS = expr})
  where (u, expr) = visitExpr altRHS

visitModule :: Module -> Module
visitModule m@Module{..} = m {moduleTopBindings = fmap visitTopBinding moduleTopBindings}

visitTopBinding :: TopBinding -> TopBinding
visitTopBinding t = case t of
  StgTopStringLit{} -> t
  StgTopLifted b
    | (u, b') <- visitBinding b
    -> if Set.null u
        then StgTopLifted b'
        else error $ "scope error in: " ++ show b ++ "\n" ++ show u

visitBinding :: Binding -> (UsedLocal, Binding)
visitBinding = \case
  StgNonRec b rhs -> (remove [b] u, StgNonRec b rhs') where (u, rhs') = visitRhs rhs
  StgRec l        -> (u1, StgRec l')
                      where
                        (l', u1) = foldr go ([], Set.empty) l
                        go (b, rhs) (xs, u0) =
                          let (u, rhs') = visitRhs rhs
                          in ((b, rhs'):xs, remove [b] $ Set.union u0 u)

visitRhs :: Rhs -> (UsedLocal, Rhs)
visitRhs = \case
  StgRhsClosure _ update args expr ->
    let (u, expr')  = visitExpr expr
        u'          = remove args u
        freeVars    = fmap unId $ Set.toList u'
    in (u', StgRhsClosure freeVars update args expr')

  StgRhsCon dc args -> (u, StgRhsCon dc args) where u = mkUsedLocal [b | StgVarArg b <- args]

visitExpr :: Expr -> (UsedLocal, Expr)
visitExpr e = case e of
  StgApp f args -> (mkUsedLocal $ f : [b | StgVarArg b <- args], e)

  StgLit{} -> (Set.empty, e)

  StgConApp _ args _  -> (mkUsedLocal [b | StgVarArg b <- args], e)

  StgOpApp _ args _ _ -> (mkUsedLocal [b | StgVarArg b <- args], e)

  StgCase expr b aty alts ->
    let (u0, expr') = visitExpr expr
        (uA, alts') = unzip $ fmap visitAlt alts
        u = remove [b] $ Set.unions $ u0 : uA
    in (u, StgCase expr' b aty alts')

  StgLet b expr ->
    let (uE, expr') = visitExpr expr
        (uB, b')    = visitBinding b
        u = remove (getBindingBinders b) $ Set.union uE uB
    in (u, StgLet b' expr')

  StgLetNoEscape b expr ->
    let (uE, expr') = visitExpr expr
        (uB, b')    = visitBinding b
        u = remove (getBindingBinders b) $ Set.union uE uB
    in (u, StgLetNoEscape b' expr')

  StgTick t expr ->
    let (u, expr') = visitExpr expr
    in (u, StgTick t expr')

getBindingBinders :: Binding -> [Binder]
getBindingBinders = \case
  StgNonRec b _ -> [b]
  StgRec l      -> fmap fst l
