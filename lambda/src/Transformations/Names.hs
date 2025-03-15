{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances #-}
module Transformations.Names where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State

import Lambda.Name

-- name monad

data NameEnv
  = NameEnv
  { namePool  :: Map Name Int
  , nameSet   :: Set Name
  }

type NameM = State NameEnv

deriveNewName :: Name -> NameM Name
deriveNewName name = do
  (newName, conflict) <- state $ \env@NameEnv{..} ->
    let idx = Map.findWithDefault 0 name namePool
        new = packName $ printf "%s_%d" name idx
    in  ( (new, Set.member new nameSet)
        , env {namePool = Map.insert name (succ idx) namePool, nameSet = Set.insert new nameSet}
        )
  if conflict
    then deriveNewName name
    else pure newName
