{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Analysis.ForeignInfo
  ( ForeignInfo(..)
  , getForeignInfo
  , getForeignInfos
  , unionForeignInfo
  , emptyForeignInfo
  ) where

import Control.Monad
import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Stg.Syntax

data ForeignInfo
  = ForeignInfo
  { fiLitLabels     :: !(Map Lit Int)
  , fiPrimCalls     :: !(Map PrimCall Int)
  , fiForeignCalls  :: !(Map ForeignCall Int)
  , fiPrimOps       :: !(Map Name Int)
  , fiLiterals      :: !(Map Lit Int)
  , fiTopStrings    :: !(Map Name Int)
  }
  deriving (Show, Eq, Ord)

emptyForeignInfo :: ForeignInfo
emptyForeignInfo = ForeignInfo
  { fiLitLabels     = Map.empty
  , fiPrimCalls     = Map.empty
  , fiForeignCalls  = Map.empty
  , fiPrimOps       = Map.empty
  , fiLiterals      = Map.empty
  , fiTopStrings    = Map.empty
  }

unionForeignInfo :: ForeignInfo -> ForeignInfo -> ForeignInfo
unionForeignInfo (ForeignInfo litA primA callA opA lA sA) (ForeignInfo litB primB callB opB lB sB) =
  ForeignInfo
    (Map.unionWith (+) litA  litB)
    (Map.unionWith (+) primA primB)
    (Map.unionWith (+) callA callB)
    (Map.unionWith (+) opA   opB)
    (Map.unionWith (+) lA lB)
    (Map.unionWith (+) sA sB)

getForeignInfos :: [Module] -> ForeignInfo
getForeignInfos l = execState (mapM_ visitModule l) emptyForeignInfo

getForeignInfo :: Module -> ForeignInfo
getForeignInfo m = execState (visitModule m) emptyForeignInfo

-- utility functions

type M = State ForeignInfo

addLitLabel :: Lit -> M ()
addLitLabel x = do
  m <- gets fiLitLabels
  modify' $ \s -> s {fiLitLabels = Map.insertWith (\a b -> (+) a $! b) x 1 m}

addPrimCall :: PrimCall -> M ()
addPrimCall x = do
  m <- gets fiPrimCalls
  modify' $ \s -> s {fiPrimCalls = Map.insertWith (\a b -> (+) a $! b) x 1 m}

addForeignCall :: ForeignCall -> M ()
addForeignCall x = do
  m <- gets fiForeignCalls
  modify' $ \s -> s {fiForeignCalls = Map.insertWith (\a b -> (+) a $! b) x 1 m}

addPrimOp :: Name -> M ()
addPrimOp x = do
  m <- gets fiPrimOps
  modify' $ \s -> s {fiPrimOps = Map.insertWith (\a b -> (+) a $! b) x 1 m}

addLit :: Lit -> M ()
addLit x = do
  m <- gets fiLiterals
  modify' $ \s -> s {fiLiterals = Map.insertWith (\a b -> (+) a $! b) x 1 m}

addString :: Name -> M ()
addString x = do
  m <- gets fiTopStrings
  modify' $ \s -> s {fiTopStrings = Map.insertWith (\a b -> (+) a $! b) x 1 m}


-----------------

visitModule :: Module -> M ()
visitModule Module{..} = mapM_ visitTopBinding moduleTopBindings

visitTopBinding :: TopBinding -> M ()
visitTopBinding = \case
  StgTopStringLit _ str -> addString str
  StgTopLifted b        -> visitBinding b

visitBinding :: Binding -> M ()
visitBinding = \case
  StgNonRec _ rhs -> visitRhs rhs
  StgRec l -> mapM_ (visitRhs . snd) l

visitRhs :: Rhs -> M ()
visitRhs = \case
  StgRhsClosure _ _ _ expr  -> visitExpr expr
  StgRhsCon _ args          -> mapM_ visitArg args

visitExpr :: Expr -> M ()
visitExpr e = case e of
  StgApp _ args -> mapM_ visitArg args

  StgLit l -> visitLit l

  StgConApp _ args _  -> mapM_ visitArg args

  StgOpApp op args _ _ -> do
    mapM_ visitArg args
    case op of
      StgPrimOp p       -> addPrimOp p
      StgPrimCallOp pc  -> addPrimCall pc
      StgFCallOp fc     -> addForeignCall fc

  StgCase expr _ _ alts -> do
    visitExpr expr
    mapM_ visitAlt alts

  StgLet b expr -> do
    visitExpr expr
    visitBinding b

  StgLetNoEscape b expr -> do
    visitExpr expr
    visitBinding b

  StgTick _ expr -> visitExpr expr

visitAlt :: Alt -> M ()
visitAlt Alt{..} = visitExpr altRHS

visitArg :: Arg -> M ()
visitArg = \case
  StgVarArg{} -> pure ()
  StgLitArg l -> visitLit l

visitLit :: Lit -> M ()
visitLit l = do
  addLit l
  case l of
    LitLabel{}  -> addLitLabel l
    _           -> pure ()
