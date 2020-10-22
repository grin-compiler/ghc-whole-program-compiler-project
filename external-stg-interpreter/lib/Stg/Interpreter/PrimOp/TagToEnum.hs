{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.TagToEnum where

import Data.List (findIndex)
import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV :: Integer -> Atom
pattern IntV i = Literal (LitNumber LitNumInt i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- dataToTag# :: a -> Int#  -- Zero-indexed; the first constructor has tag zero
  ("dataToTag#", [addr@HeapPtr{}]) -> do
    (Con dataCon []) <- readHeapCon addr
    case findIndex (\d -> dcId d == dcId dataCon) (tcDataCons (dcTyCon dataCon)) of
      Nothing -> stgErrorM $ "Data constructor is not found for " ++ show (dcId dataCon)
      Just i -> pure [IntV $ fromIntegral i]

  -- tagToEnum# :: Int# -> a
  ("tagToEnum", [IntV i]) -> do
    Just tyc <- pure tc
    let dc = tcDataCons tyc !! (fromIntegral i)
    loc <- allocAndStore (Con dc [])
    pure [HeapPtr loc]

  _ -> fallback op args t tc
