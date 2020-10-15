{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter where

import Control.Monad.State

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

import Stg.Syntax
import Stg.Program

import Stg.Interpreter.Base

import qualified Stg.Interpreter.PrimOp.Addr          as PrimAddr
import qualified Stg.Interpreter.PrimOp.Array         as PrimArray
import qualified Stg.Interpreter.PrimOp.ByteArray     as PrimByteArray
import qualified Stg.Interpreter.PrimOp.Char          as PrimChar
import qualified Stg.Interpreter.PrimOp.Concurrency   as PrimConcurrency
import qualified Stg.Interpreter.PrimOp.Exceptions    as PrimExceptions
import qualified Stg.Interpreter.PrimOp.Word          as PrimWord
import qualified Stg.Interpreter.PrimOp.Word8         as PrimWord8
import qualified Stg.Interpreter.PrimOp.Word16        as PrimWord16
import qualified Stg.Interpreter.PrimOp.Int           as PrimInt
import qualified Stg.Interpreter.PrimOp.Int8          as PrimInt8
import qualified Stg.Interpreter.PrimOp.Int16         as PrimInt16
import qualified Stg.Interpreter.PrimOp.MutVar        as PrimMutVar
import qualified Stg.Interpreter.PrimOp.MVar          as PrimMVar
import qualified Stg.Interpreter.PrimOp.Narrowings    as PrimNarrowings
import qualified Stg.Interpreter.PrimOp.StablePointer as PrimStablePointer
import qualified Stg.Interpreter.PrimOp.WeakPointer   as PrimWeakPointer

{-
  Q: what is the operational semantic of StgApp
  A: check slowCall :: CmmExpr -> [StgArg] -> FCode ReturnKind semantics
  done - read STG eval apply paper

  TODO:
    - refresh all STG uniques to be globally unique
    - collect free variables for closures

  EVAL:
    - setup top level env (TopEnv) ; add top level bindings to it
    - call main
-}


evalArg :: Arg -> M Atom
evalArg = \case
  StgLitArg l -> pure $ Literal l
  StgVarArg b -> lookupEnv b

builtinStgEval :: Atom -> M [Atom]
builtinStgEval a = do
  o <- readHeap a
  case o of
    Con{}       -> pure [a]
    Blackhole t -> stgErrorM $ "blackhole ; loop in evaluation of : " ++ show t
    Closure{..}
      | hoCloMissing /= 0
      -> pure [a]

      | otherwise
      -> do
        let StgRhsClosure uf params e = hoCloBody
            HeapPtr l = a
            argsM = zipWithM_ addEnv params hoCloArgs
        -- TODO: env or free var handling
        case uf of
          ReEntrant -> do
            -- closure may be entered multiple times, but should not be updated or blackholed.
            withEnv hoName hoEnv (argsM >> evalExpr e)
          Updatable -> do
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            store l (Blackhole o)
            result <- withEnv hoName hoEnv (argsM >> evalExpr e)
            update a result
          SingleEntry -> do
            -- closure will only be entered once, and so need not be updated but may safely be blackholed.
            store l (Blackhole o)
            withEnv hoName hoEnv (argsM >> evalExpr e)

builtinStgApply :: Atom -> [Atom] -> M [Atom]
builtinStgApply a args = do
  let argCount      = length args
      HeapPtr addr  = a
  o <- readHeap a
  case o of
    Con{}       -> stgErrorM $ "unexpexted con at apply: " ++ show o
    Blackhole t -> stgErrorM $ "blackhole ; loop in application of : " ++ show t
    Closure{..}
      -- under saturation
      | hoCloMissing - argCount > 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        pure [HeapPtr newAp]

      -- over saturation
      | hoCloMissing - argCount < 0
      -> do
        let (satArgs, remArgs) = splitAt hoCloMissing args
        builtinStgApply a satArgs >>= \case
          [f] -> builtinStgApply f remArgs
          r   -> stgErrorM $ "invalid over-application result: " ++ show r

      -- saturation
      | hoCloMissing - argCount == 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        builtinStgEval (HeapPtr newAp)

evalExpr :: Expr -> M [Atom]
evalExpr = \case
  StgTick _ e       -> evalExpr e
  StgLit l          -> pure [Literal l] -- FIXME: LitString?? is it addr??
  StgConApp dc l _
    -- HINT: make and return unboxed tuple
    | UnboxedTupleCon{} <- dcRep dc
    -> mapM evalArg l   -- Q: is this only for unboxed tuple? could datacon be heap allocated?

    -- HINT: create boxed datacon on the heap
    | otherwise
    -> do
      args <- mapM evalArg l
      loc <- allocAndStore (Con dc args)
      pure [HeapPtr loc]

  StgLet b e -> do
    declareBinding b
    evalExpr e

  StgLetNoEscape b e -> do -- TODO: do not allocate closure on heap, instead put into env (stack) allocated closure ; model stack allocated heap objects
    declareBinding b
    evalExpr e

  -- var
  StgApp i [] _t _ -> case binderType i of

    SingleValue LiftedRep -> do
      -- HINT: must be HeapPtr ; read heap ; check if Con or Closure ; eval if Closure ; return HeapPtr if Con
      v <- lookupEnv i
      builtinStgEval v

    SingleValue _ -> do
      -- HINT: unlifted and literals ; Unlifted must be a HeapPtr that points to a Con
      v <- lookupEnv i
      pure [v]

    r -> stgErrorM $ "unsupported var rep: " ++ show r -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  StgApp i l _t _ -> case binderType i of
    SingleValue _ -> do
      args <- mapM evalArg l
      v <- lookupEnv i
      builtinStgApply v args

    r -> stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e resultId aty alts -> do
    result <- evalExpr e
    case aty of
      AlgAlt tc -> do
        let [v] = result
        addEnv resultId v -- HINT: bind the result
        con <- readHeapCon v
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstCon con $ al ++ [d]
          _ -> matchFirstCon con alts

      PrimAlt r -> do
        let [lit] = result
        addEnv resultId lit -- HINT: bind the result
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstLit lit $ al ++ [d]
          _ -> matchFirstLit lit alts

      MultiValAlt _n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [Alt{..}] = alts
        zipWithM_ addEnv altBinders result
        evalExpr altRHS

      PolyAlt -> do
        let [v] = result
        addEnv resultId v -- HINT: bind the result
        let [Alt{..}] = alts
        zipWithM_ addEnv altBinders result
        evalExpr altRHS

  StgOpApp (StgPrimOp op) l t tc -> do
    args <- mapM evalArg l
    evalPrimOp op args t tc

  StgOpApp fop@(StgFCallOp (ForeignCall{..})) l t _tc -> do
    args <- mapM evalArg l
    case foreignCTarget of
      StaticTarget _ "localeEncoding" _ _ -> pure [Literal LitNullAddr]
      StaticTarget _ "getProgArgv" _ _ -> pure []
      StaticTarget _ "hs_free_stable_ptr" _ _ -> pure []
      StaticTarget _ "rts_setMainThread" _ _ -> pure []
      StaticTarget _ "getOrSetGHCConcSignalSignalHandlerStore" _ _ -> pure [head args] -- WTF!
      StaticTarget _ "stg_sig_install" _ _ -> pure [Literal (LitNumber LitNumInt 0)]
      StaticTarget _ "hs_iconv_open" _ _ -> pure [Literal (LitNumber LitNumInt 0)]
      _ -> stgErrorM $ "unsupported StgFCallOp: " ++ show fop ++ " :: " ++ show t ++ " args: " ++ show args

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t

{-
          (State# RealWorld -> (# State# RealWorld, a #) )
       -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
       -> State# RealWorld
       -> (# State# RealWorld, a #)
-}

{-
  | StgOpApp    StgOp         -- Primitive op or foreign call
                [Arg' idOcc]  -- Saturated.
                Type          -- result type
                (Maybe tcOcc) -- result type name (required for tagToEnum wrapper generator)


data StgOp
  = StgPrimOp     !Name
  | StgPrimCallOp !PrimCall
  | StgFCallOp    !ForeignCall

data PrimCall = PrimCall !BS8.ByteString !UnitId

data ForeignCall
  = ForeignCall
  { foreignCTarget  :: !CCallTarget
  , foreignCConv    :: !CCallConv
  , foreignCSafety  :: !Safety
  }

data CCallTarget
  = StaticTarget !SourceText !BS8.ByteString !(Maybe UnitId) !Bool
  | DynamicTarget

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv

-}

matchFirstLit :: Atom -> [Alt] -> M [Atom]
matchFirstLit (Literal lit) alts = case head $ [a | a@Alt{..} <- alts, matchLit lit altCon] ++ (error $ "no lit match" ++ show (lit, map altCon alts)) of
  Alt{..} -> do
    evalExpr altRHS
matchFirstLit a ([Alt AltDefault _ rhs]) = evalExpr rhs
matchFirstLit l alts = error $ "no lit match" ++ show (l, map altCon alts)

matchLit :: Lit -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  -> False
  AltLit l      -> a == l
  AltDefault    -> True

matchFirstCon :: HeapObject -> [Alt] -> M [Atom]
matchFirstCon (Con dc args) alts = case head [a | a@Alt{..} <- alts, matchCon dc altCon] of
  Alt{..} -> do
    zipWithM_ addEnv altBinders args
    evalExpr altRHS

matchCon :: DataCon -> AltCon -> Bool
matchCon a = \case
  AltDataCon dc -> dataConUniqueName a == dataConUniqueName dc
  AltLit{}      -> False
  AltDefault    -> True

declareBinding :: Binding -> M ()
declareBinding = \case
  StgNonRec i rhs -> do
    a <- freshHeapAddress
    addEnv i (HeapPtr a)
    storeRhs i a rhs
  StgRec l -> do
    ls <- forM l $ \(i, _) -> do
      a <- freshHeapAddress
      addEnv i (HeapPtr a)
      pure a
    forM_ (zip ls l) $ \(a, (i, rhs)) -> do
      storeRhs i a rhs

storeRhs :: Binder -> Addr -> Rhs -> M ()
storeRhs i a = \case
  StgRhsCon dc l -> do
    args <- mapM evalArg l
    store a (Con dc args)

  cl@(StgRhsClosure _ l _) -> do
    env <- gets ssEnv -- TODO: pruning
    store a (Closure (Id i) cl env [] (length l))

-----------------------

declareTopBindings :: [Module] -> M ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ (concatMap moduleTopBindings) mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  forM_ strings $ \(StgTopStringLit b str) -> do
    addEnv b (StringPtr 0 (str <> "\0")) -- FIXME: StringPtr or Lit???)

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l
  addrList <- forM bindings $ \(i, _) -> do
    a <- freshHeapAddress
    addEnv i (HeapPtr a)
    pure a

  forM_ (zip addrList bindings) $ \(a, (i, rhs)) -> do
    storeRhs i a rhs

test :: IO ()
test = do
  mods <- getFullpakModules "minigame.fullpak"
  let run = do
        declareTopBindings mods
        rootMain <- unId . head . Map.keys <$> gets ssEnv
        evalExpr (StgApp rootMain [StgLitArg LitNullAddr] (SingleValue VoidRep) mempty)

  let s@StgState{..} = execState run emptyStgState

  putStrLn $ unlines $ [BS8.unpack $ binderUniqueName b | Id b <- Map.keys ssEnv]
  print ssNextAddr
  print $ head $ Map.toList ssEnv
  -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main

---------------------- primops

evalPrimOp :: Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp =
  PrimAddr.evalPrimOp $
  PrimArray.evalPrimOp $
  PrimByteArray.evalPrimOp $
  PrimChar.evalPrimOp $
  PrimConcurrency.evalPrimOp $
  PrimExceptions.evalPrimOp builtinStgApply $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
  PrimMutVar.evalPrimOp $
  PrimMVar.evalPrimOp $
  PrimNarrowings.evalPrimOp $
  PrimStablePointer.evalPrimOp $
  PrimWeakPointer.evalPrimOp $
  PrimWord16.evalPrimOp $
  PrimWord8.evalPrimOp $
  PrimWord.evalPrimOp $
  unsupported where
    unsupported op args _t _tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
