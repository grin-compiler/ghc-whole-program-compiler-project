{-# LANGUAGE LambdaCase, FlexibleContexts, RecordWildCards #-}
module Transformations.Util where

import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable

import Data.Map (Map)
import qualified Data.Map as Map

subst :: Ord a => Map a a -> a -> a
subst env x = Map.findWithDefault x x env

-- monadic recursion schemes
--  see: https://jtobin.io/monadic-recursion-schemes

cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t ->  m a
cataM alg = c where
    c = alg <=< traverse c . project

anaM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where
  a = (pure . embed) <=< traverse a <=< coalg

paraM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (t, a) -> m a) -> t -> m a
paraM alg = p where
  p   = alg <=< traverse f . project
  f t = liftM2 (,) (pure t) (p t)

apoM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = a where
  a = (pure . embed) <=< traverse f <=< coalg
  f = either pure a

hyloM
  :: (Monad m, Traversable t)
  => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where h = alg <=< traverse h <=< coalg

histoM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (Cofree (Base t) a) -> m a) -> t -> m a
histoM h = pure . extract <=< worker where
  worker = f <=< traverse worker . project
  f x = (:<) <$> h x <*> pure x

