module Stg.Tickish where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (Monad (..), mapM_, sequence_)
import           Control.Monad.RWS   (MonadReader (..), MonadWriter (..), RWS, evalRWS)

import           Data.Function       (const, ($))
import           Data.Int            (Int)
import           Data.List           (zip)
import           Data.Maybe          (Maybe (..))
import           Data.Tuple          (snd, uncurry)

import           GHC.Err             (error)

import           Stg.IRLocation      (StgPoint (..), binderToStgId)
import           Stg.Syntax          (Alt, Alt' (..), Binder, Binding, Binding' (..), Expr, Expr' (..), Id (..), Module,
                                      Module' (..), Rhs, Rhs' (..), Tickish, TopBinding, TopBinding' (..))

type M = RWS (Maybe StgPoint) [(StgPoint, Tickish)] ()

withStgPoint :: StgPoint -> M () -> M ()
withStgPoint sp = local (const $ Just sp)

getStgPoint :: M StgPoint
getStgPoint = ask >>= \case
  Nothing -> error "missing stg point"
  Just sp -> pure sp

visitTopBinding :: TopBinding -> M ()
visitTopBinding = \case
  StgTopLifted b    -> visitBinding b
  StgTopStringLit{} -> pure ()

visitBinding :: Binding -> M ()
visitBinding = \case
  StgNonRec b r  -> visitRhs b r
  StgRec bs      -> mapM_ (uncurry visitRhs) bs

visitRhs :: Binder -> Rhs -> M ()
visitRhs rhsBinder = \case
  StgRhsClosure _ _ _ e -> withStgPoint (SP_RhsClosureExpr $ binderToStgId rhsBinder) $ visitExpr e
  StgRhsCon{}           -> pure ()

visitExpr :: Expr -> M ()
visitExpr expr = do
  stgPoint <- getStgPoint
  case expr of
    StgLit{}            -> pure ()
    StgApp{}            -> pure ()
    StgOpApp{}          -> pure ()
    StgConApp{}         -> pure ()
    StgCase x b _ alts  -> do
                            withStgPoint (SP_CaseScrutineeExpr $ binderToStgId b) $ visitExpr x
                            sequence_ [visitAlt (Id b) idx a | (idx, a) <- zip [0..] alts]
    StgLet b e          -> do
                            visitBinding b
                            withStgPoint (SP_LetExpr stgPoint) $ visitExpr e
    StgLetNoEscape b e  -> do
                            visitBinding b
                            withStgPoint (SP_LetNoEscapeExpr stgPoint) $ visitExpr e
    StgTick tickish e   -> do
                            tell [(stgPoint, tickish)]
                            visitExpr e

visitAlt :: Id -> Int -> Alt -> M ()
visitAlt (Id scrutBinder) idx (Alt _con _bndrs rhs) = do
  withStgPoint (SP_AltExpr (binderToStgId scrutBinder) idx) $ visitExpr rhs

collectTickish :: Module -> [(StgPoint, Tickish)]
collectTickish m = snd $ evalRWS (mapM_ visitTopBinding $ moduleTopBindings m) Nothing ()
