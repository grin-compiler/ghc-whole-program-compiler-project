{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings #-}
module Stg.GHC.ToStg
  ( toStg
  , StgModule(..)
  ) where

-- Compiler
import GHC
import GHC.Driver.Types
import GHC.Utils.Outputable

-- Stg Types
import GHC.Unit.Module
import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Var
import GHC.Types.Unique
import GHC.Types.Name.Occurrence as OccName
import GHC.Stg.Syntax

import GHC.Types.CostCentre
import GHC.Types.ForeignCall
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Core (AltCon(..))

import GHC.Builtin.PrimOps
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Types.Literal
import GHC.Types.Id.Make
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Type

import Control.Monad.State
import Data.List (partition)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Stg.Syntax as Ext

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

-- primop utility

primOpMap :: Map Ext.Name PrimOp
primOpMap = Map.fromList [(BS8.pack . occNameString . primOpOcc $ op, op) | op <- allThePrimOps]

ambiguousPrimOps :: Map Ext.Name [Int]
ambiguousPrimOps = Map.filter (\a -> length a > 1) $
  Map.unionsWith (++) [Map.singleton (BS8.pack . occNameString . primOpOcc $ op) [primOpTag op] | op <- allThePrimOps]

-- tycon + datacon utility

-- minimalistic type construction for GHC/STG codegen
simpleDataCon :: TyCon -> Name -> [PrimRep] -> ConTag -> Name -> DataCon
simpleDataCon tc name args tag workerName = dataCon
  where
    dataCon       = mkDataCon
                      name False (error "TyConRepName") [] [] [] [] [] [] []
                      (map primRepToType args) ({-error "Original result type"-}primRepToType LiftedRep) (error "RuntimeRepInfo")
                      tc tag [] workerId NoDataConRep
    workerId      = mkDataConWorkId workerName dataCon

simpleTyCon :: Name -> [DataCon] -> TyCon
simpleTyCon name dataCons = mkAlgTyCon name [] {-(error "Kind")-}liftedTypeKind [] Nothing [] (mkDataTyConRhs dataCons) (VanillaAlgTyCon (error "TyConRepName")) False

primRepToType :: PrimRep -> Type
primRepToType = \case
  VoidRep     -> voidPrimTy
  Int8Rep     -> int8PrimTy
  Int16Rep    -> int16PrimTy
  Int32Rep    -> int32PrimTy
  Int64Rep    -> int64PrimTy
  IntRep      -> intPrimTy
  Word8Rep    -> word8PrimTy
  Word16Rep   -> word16PrimTy
  Word32Rep   -> word32PrimTy
  Word64Rep   -> word64PrimTy
  WordRep     -> wordPrimTy
  AddrRep     -> addrPrimTy
  FloatRep    -> floatPrimTy
  DoubleRep   -> doublePrimTy
  r           -> anyTypeOfKind . tYPE . primRepToRuntimeRep $ r

---------------

data Env
  = Env
  { envADTTyConMap  :: Map Ext.TyConId TyCon
  , envDataConMap   :: Map Ext.DataConId DataCon
  , envIdMap        :: Map Ext.BinderId Id
  , envNextUnique   :: !Int
  , envThisUnitId   :: !Ext.UnitId
  , envThisModule   :: !Ext.ModuleName
  }

emptyEnv :: Ext.UnitId -> Ext.ModuleName -> Env
emptyEnv u m = Env
  { envADTTyConMap  = mempty
  , envDataConMap   = mempty
  , envIdMap        = mempty
  , envNextUnique   = 0
  , envThisUnitId   = u
  , envThisModule   = m
  }

type M = State Env

getFreshUnique :: M Unique
getFreshUnique = state $ \env@Env{..} ->
  let u = mkUnique 'W' envNextUnique
  in (u, env {envNextUnique = succ envNextUnique})

---------------
{-
  TODO:
    - cache things
-}

setAlgTyCons :: [Ext.TyCon] -> M ()
setAlgTyCons tyCons = do
  forM_ tyCons $ \Ext.TyCon{..} -> do
    tyConName <- getFreshExternalName OccName.tcName tcUnitId tcModule tcName
    dcNames <- forM tcDataCons $ \Ext.DataCon{..} -> do
      conName <- getFreshExternalName OccName.dataName dcUnitId dcModule dcName
      let Ext.Binder{..} = dcWorker
      workerName <- getFreshExternalName OccName.varName binderUnitId binderModule binderName
      pure (conName, workerName)

    let dataCons :: [(Ext.DataCon, DataCon)]
        dataCons = [(edc, simpleDataCon tyCon conName (getConRep dcRep) tag workerName) | ((conName, workerName), edc@Ext.DataCon{..}, tag) <- zip3 dcNames tcDataCons [1..]]

        tyCon :: TyCon
        tyCon = simpleTyCon tyConName $ map snd dataCons

        getConRep :: Ext.DataConRep -> [PrimRep]
        getConRep = \case
          Ext.UnboxedTupleCon{} -> error $ "UnboxedTupleCon in alg TyCon: " ++ show (tcUnitId, tcModule, tcName)
          Ext.AlgDataCon l      -> map cvtPrimRep l

    modify' $ \env@Env{..} -> env { envADTTyConMap = Map.insert tcId tyCon envADTTyConMap
                                  , envDataConMap = foldr (\(k, v) m -> Map.insert k v m) envDataConMap [(dcId, dc) | (Ext.DataCon{..}, dc) <- dataCons]
                                  }
getFreshName :: NameSpace -> Ext.UnitId -> Ext.ModuleName -> Ext.Name -> M Name
getFreshName ns uid mod name = do
  uniq <- getFreshUnique
  Env{..} <- get
  let isExternal = uid /= envThisUnitId || mod /= envThisModule
  if isExternal
    then do
      modl <- getBinderModule uid mod
      pure $ mkExternalName uniq modl (mkOccNameFS ns $ mkFastStringByteString name) noSrcSpan
    else do
      pure $ mkInternalName uniq (mkOccNameFS ns $ mkFastStringByteString name) noSrcSpan
      {-
      modl <- getBinderModule uid mod
      pure $ mkExternalName uniq modl (mkOccNameFS ns $ mkFastStringByteString $ name Prelude.<> (BS8.pack $ show uniq)) noSrcSpan
      -}


getFreshExternalName :: NameSpace -> Ext.UnitId -> Ext.ModuleName -> Ext.Name -> M Name
getFreshExternalName ns uid mod name = do
  uniq <- getFreshUnique
  modl <- getBinderModule uid mod
  pure $ mkExternalName uniq modl (mkOccNameFS ns $ mkFastStringByteString name) noSrcSpan

-- TODO: cache what needs to be cached
getBinderModule :: Ext.UnitId -> Ext.ModuleName -> M Module
getBinderModule uid mod = do
  let u = cvtUnitId uid
      m = cvtModuleName mod
  pure $ mkModule u m

cvtIdDetails :: Ext.IdDetails -> Ext.Name -> M IdDetails
cvtIdDetails details uname = do
  dcMap <- gets envDataConMap
  pure $ case details of
    Ext.VanillaId       -> VanillaId
    Ext.FExportedId     -> VanillaId
    Ext.RecSelId        -> RecSelId       (error "Ext.RecSelId sel_tycon") (error "Ext.RecSelId sel_naughty")
    Ext.DataConWorkId d -> DataConWorkId  $ Map.findWithDefault (error $ "Ext.DataConWorkId DataCon: " ++ show d ++ ", binder name: " ++ show uname) d dcMap
    Ext.DataConWrapId d -> DataConWrapId  $ Map.findWithDefault (error $ "Ext.DataConWrapId DataCon: " ++ show d ++ ", binder name: " ++ show uname) d dcMap
    Ext.ClassOpId       -> ClassOpId      (error "Ext.ClassOpId Class")
    Ext.PrimOpId        -> PrimOpId       (error "Ext.PrimOpId PrimOp")
    Ext.FCallId         -> FCallId        (error "Ext.FCallId ForeignCall")
    Ext.TickBoxOpId     -> TickBoxOpId    (error "Ext.TickBoxOpId TickBoxOp")
    Ext.DFunId          -> DFunId         (error "Ext.DFunId Bool")
    Ext.CoVarId         -> CoVarId
    Ext.JoinId ar       -> JoinId ar

cvtId :: Ext.Binder -> M Id
cvtId b@Ext.Binder{..} = do
  Env{..} <- get
  case Map.lookup binderId envIdMap of
    Just i  -> pure i
    Nothing -> cvtNewId b

rootMainBinderId :: Ext.BinderId
rootMainBinderId = Ext.BinderId $ Ext.Unique a b
  where (a,b) = unpkUnique rootMainKey

cvtIdDef :: Ext.Binder -> M Id
cvtIdDef b
  -- special case for :Main.main that has wired-in rootMainKey unique value
  | Ext.binderId b == rootMainBinderId
  = cvtId $ b {Ext.binderModule = Ext.ModuleName ":Main"}

  -- always alloc new uniques for local binders
  | Ext.binderScope b == Ext.LocalScope
  = cvtNewId b

  -- keep unique values, globals are never shadowed
  | otherwise
  = cvtId b


cvtNewId :: Ext.Binder -> M Id
cvtNewId Ext.Binder{..} = do
  details <- cvtIdDetails binderDetails binderUniqueName
  nameId <- case binderScope of
    s | s == Ext.LocalScope || s == Ext.GlobalScope -> do
      name <- getFreshName OccName.varName binderUnitId binderModule binderName
      pure $ mkLocalId name (cvtPrimRepType binderType)
    _ -> do
      name <- getFreshExternalName OccName.varName binderUnitId binderModule binderName
      pure $ mkVanillaGlobal name (cvtPrimRepType binderType)

  let finalId = setIdDetails nameId details

  state $ \env@Env{..} -> (finalId, env {envIdMap = Map.insert binderId finalId envIdMap})

cvtUnitId :: Ext.UnitId -> Unit
cvtUnitId = fsToUnit . mkFastStringByteString . Ext.getUnitId

cvtModuleName :: Ext.ModuleName -> ModuleName
cvtModuleName = mkModuleNameFS . mkFastStringByteString . Ext.getModuleName

cvtPrimRep :: Ext.PrimRep -> PrimRep
cvtPrimRep = \case
  Ext.VoidRep     -> VoidRep
  Ext.LiftedRep   -> LiftedRep
  Ext.UnliftedRep -> UnliftedRep
  Ext.Int8Rep     -> Int8Rep
  Ext.Int16Rep    -> Int16Rep
  Ext.Int32Rep    -> Int32Rep
  Ext.Int64Rep    -> Int64Rep
  Ext.IntRep      -> IntRep
  Ext.Word8Rep    -> Word8Rep
  Ext.Word16Rep   -> Word16Rep
  Ext.Word32Rep   -> Word32Rep
  Ext.Word64Rep   -> Word64Rep
  Ext.WordRep     -> WordRep
  Ext.AddrRep     -> AddrRep
  Ext.FloatRep    -> FloatRep
  Ext.DoubleRep   -> DoubleRep
  Ext.VecRep i e  -> VecRep i $ cvtPrimElemRep e

cvtPrimElemRep :: Ext.PrimElemRep -> PrimElemRep
cvtPrimElemRep = \case
  Ext.Int8ElemRep   -> Int8ElemRep
  Ext.Int16ElemRep  -> Int16ElemRep
  Ext.Int32ElemRep  -> Int32ElemRep
  Ext.Int64ElemRep  -> Int64ElemRep
  Ext.Word8ElemRep  -> Word8ElemRep
  Ext.Word16ElemRep -> Word16ElemRep
  Ext.Word32ElemRep -> Word32ElemRep
  Ext.Word64ElemRep -> Word64ElemRep
  Ext.FloatElemRep  -> FloatElemRep
  Ext.DoubleElemRep -> DoubleElemRep

cvtPrimRepType :: Ext.Type -> Type
cvtPrimRepType = \case
  Ext.SingleValue Ext.VoidRep -> mkTupleTy Unboxed []
  Ext.SingleValue r   -> primRepToType $ cvtPrimRep r
  Ext.UnboxedTuple l  -> mkTupleTy Unboxed $ map (primRepToType . cvtPrimRep) l
  Ext.PolymorphicRep  -> mkInfForAllTy runtimeRep2TyVar
                          $ mkSpecForAllTys [openBetaTyVar]
                          $ mkTyVarTy openBetaTyVar
                          -- HINT: forall (r :: RuntimeRep) (b :: TYPE r). b

cvtADTType :: Ext.Type -> Maybe Ext.TyCon -> M Type
cvtADTType t = \case
  Nothing -> pure $ cvtPrimRepType t
  Just etc  -> gets envADTTyConMap >>= \m -> case Map.lookup (Ext.tcId etc) m of
    Nothing -> error $ "unknown ADT TyCon: " ++ show (Ext.tcUnitId etc, Ext.tcModule etc, Ext.tcName etc)
    Just tc -> pure $ mkTyConTy tc

cvtDataCon :: Ext.DataCon -> M DataCon
cvtDataCon Ext.DataCon{..} = case dcRep of
  Ext.UnboxedTupleCon l -> do
    pure $ tupleDataCon Unboxed (l `div` 2) -- TODO: make this accurate
  _ -> do
    gets envDataConMap >>= \m -> case Map.lookup dcId m of
      Nothing -> error $ "unknown DataCon: " ++ show dcName
      Just dc -> pure dc

cvtAltType :: Ext.AltType -> M AltType
cvtAltType = \case
  Ext.PolyAlt       -> pure PolyAlt
  Ext.MultiValAlt i -> pure $ MultiValAlt i
  Ext.PrimAlt r     -> pure . PrimAlt $ cvtPrimRep r
  Ext.AlgAlt etc    -> gets envADTTyConMap >>= \m -> case Map.lookup (Ext.tcId etc) m of
    Nothing -> error $ "unknown ADT TyCon: " ++ show (Ext.tcUnitId etc, Ext.tcModule etc, Ext.tcName etc)
    Just tc -> pure $ AlgAlt tc

cvtSourceText :: Ext.SourceText -> SourceText
cvtSourceText = \case
  Ext.SourceText s  -> SourceText (BS8.unpack s)
  Ext.NoSourceText  -> NoSourceText

cvtForeignCall :: Ext.ForeignCall -> ForeignCall
cvtForeignCall Ext.ForeignCall{..} = CCall $ CCallSpec target callconv safety where
  target = case foreignCTarget of
    Ext.StaticTarget s l u b  -> StaticTarget (cvtSourceText s) (mkFastStringByteString l) (fmap cvtUnitId u) b
    Ext.DynamicTarget         -> DynamicTarget

  callconv = case foreignCConv of
    Ext.CCallConv           -> CCallConv
    Ext.CApiConv            -> CApiConv
    Ext.StdCallConv         -> StdCallConv
    Ext.PrimCallConv        -> PrimCallConv
    Ext.JavaScriptCallConv  -> JavaScriptCallConv

  safety = case foreignCSafety of
    Ext.PlaySafe          -> PlaySafe
    Ext.PlayInterruptible -> PlayInterruptible
    Ext.PlayRisky         -> PlayRisky

cvtPrimCall :: Ext.PrimCall -> PrimCall
cvtPrimCall (Ext.PrimCall lbl uid) = PrimCall (mkFastStringByteString lbl) (cvtUnitId uid)

-- creates a function type for FFI cmm codegen
--  the result type does not matter
mkStgFArgType :: [Ext.Arg] -> Type
mkStgFArgType args = mkVisFunTys (map getArgType args) intTy where
  getArgType :: Ext.Arg -> Type
  getArgType = \case
    Ext.StgLitArg{} -> intTy
    Ext.StgVarArg i -> case head . BS8.words $ Ext.binderTypeSig i of
      "Array#"              -> mkArrayPrimTy intTy
      "MutableArray#"       -> mkMutableArrayPrimTy realWorldTy intTy
      "SmallArray#"         -> mkSmallArrayPrimTy intTy
      "SmallMutableArray#"  -> mkSmallMutableArrayPrimTy realWorldTy intTy
      "ArrayArray#"         -> mkArrayArrayPrimTy
      "MutableArrayArray#"  -> mkMutableArrayArrayPrimTy realWorldTy
      "ByteArray#"          -> byteArrayPrimTy
      "MutableByteArray#"   -> mkMutableByteArrayPrimTy realWorldTy
      _ -> intTy -- HINT: the actual type does not matter

cvtOp :: [Ext.Arg] -> Ext.StgOp -> StgOp
cvtOp args = \case
  Ext.StgPrimOp op    -> case Map.lookup op primOpMap of
                          Nothing -> error $ "unknown primop: " ++ show op
                          Just o  -> StgPrimOp o
  Ext.StgPrimCallOp c -> StgPrimCallOp (cvtPrimCall c)
  Ext.StgFCallOp f    -> StgFCallOp (cvtForeignCall f) (mkStgFArgType args)

cvtLitNumType :: Ext.LitNumType -> (LitNumType, Type)
cvtLitNumType = \case
  Ext.LitNumInt     -> (LitNumInt   , intPrimTy)
  Ext.LitNumInt64   -> (LitNumInt64 , int64PrimTy)
  Ext.LitNumWord    -> (LitNumWord  , wordPrimTy)
  Ext.LitNumWord64  -> (LitNumWord64, word64PrimTy)

cvtLabelSpec :: Ext.LabelSpec -> (Maybe Int, FunctionOrData)
cvtLabelSpec = \case
  Ext.FunctionLabel mi  -> (mi, IsFunction)
  Ext.DataLabel         -> (Nothing, IsData)

cvtLit :: Ext.Lit -> Literal
cvtLit = \case
  Ext.LitChar x     -> LitChar x
  Ext.LitString x   -> LitString x
  Ext.LitNullAddr   -> LitNullAddr
  Ext.LitFloat x    -> LitFloat x
  Ext.LitDouble x   -> LitDouble x
  Ext.LitLabel x s  -> LitLabel (mkFastStringByteString x) i d where (i, d) = (cvtLabelSpec s)
  Ext.LitNumber t i -> LitNumber numTy i ty where (numTy, ty) = cvtLitNumType t

cvtAltCon :: Ext.AltCon -> M AltCon
cvtAltCon = \case
  Ext.AltDataCon dc -> DataAlt <$> cvtDataCon dc
  Ext.AltLit l      -> pure $ LitAlt (cvtLit l)
  Ext.AltDefault    -> pure $ DEFAULT

cvtAlt :: Ext.Alt -> M StgAlt
cvtAlt Ext.Alt{..} = (,,) <$> cvtAltCon altCon <*> mapM cvtIdDef altBinders <*> cvtExpr altRHS

cvtExpr :: Ext.Expr -> M StgExpr
cvtExpr = \case
  Ext.StgApp f args t (_,_,l) -> StgApp <$> cvtId f <*> cvtArgs args <*> pure (cvtPrimRepType t, BS8.unpack l)
  Ext.StgLit l                -> pure $ StgLit (cvtLit l)
  Ext.StgConApp dc args t     -> StgConApp <$> cvtDataCon dc <*> cvtArgs args <*> pure (map cvtPrimRepType t)
  Ext.StgOpApp op args t tc   -> StgOpApp (cvtOp args op) <$> cvtArgs args <*> cvtADTType t tc
  Ext.StgCase exp i at alts   -> StgCase <$> cvtExpr exp <*> cvtIdDef i <*> cvtAltType at <*> mapM cvtAlt alts
  Ext.StgLet b exp            -> StgLet noExtFieldSilent <$> cvtBinding b <*> cvtExpr exp
  Ext.StgLetNoEscape b exp    -> StgLetNoEscape noExtFieldSilent <$> cvtBinding b <*> cvtExpr exp
  Ext.StgTick _t exp          -> cvtExpr exp -- TODO: convert tickish

cvtArgs :: [Ext.Arg] -> M [StgArg]
cvtArgs = mapM cvtArg

cvtArg :: Ext.Arg -> M StgArg
cvtArg = \case
  Ext.StgVarArg i -> StgVarArg <$> cvtId i
  Ext.StgLitArg l -> pure $ StgLitArg (cvtLit l)

cvtUpdateFlag :: Ext.UpdateFlag -> UpdateFlag
cvtUpdateFlag = \case
  Ext.ReEntrant   -> ReEntrant
  Ext.Updatable   -> Updatable
  Ext.SingleEntry -> SingleEntry

cvtRhs :: Ext.Rhs -> M StgRhs
cvtRhs = \case
  Ext.StgRhsClosure _ u args exp  -> StgRhsClosure noExtFieldSilent dontCareCCS (cvtUpdateFlag u) <$> mapM cvtIdDef args <*> cvtExpr exp
  Ext.StgRhsCon dc args           -> StgRhsCon dontCareCCS <$> cvtDataCon dc <*> cvtArgs args

cvtBinding :: Ext.Binding -> M StgBinding
cvtBinding = \case
  Ext.StgNonRec i r -> StgNonRec <$> cvtIdDef i <*> cvtRhs r
  Ext.StgRec l      -> do
                        -- HINT: add new ids to the scope
                        mapM_ (cvtIdDef . fst) l
                        StgRec <$> sequence [(,) <$> cvtId i <*> cvtRhs r | (i, r) <- l]

cvtTopBinding :: Ext.TopBinding -> M StgTopBinding
cvtTopBinding = \case
  Ext.StgTopLifted b      -> StgTopLifted <$> cvtBinding b
  Ext.StgTopStringLit i s -> StgTopStringLit <$> cvtIdDef i <*> pure s

-- foreign stubs/files

cvtForeignStubs :: Ext.ForeignStubs -> ForeignStubs
cvtForeignStubs = \case
  Ext.NoStubs           -> NoStubs
  Ext.ForeignStubs{..}  -> ForeignStubs (ftext $ mkFastStringByteString fsCHeader) (ftext $ mkFastStringByteString fsCSource)

cvtForeignSrcLang :: Ext.ForeignSrcLang -> ForeignSrcLang
cvtForeignSrcLang = \case
  Ext.LangC       -> LangC
  Ext.LangCxx     -> LangCxx
  Ext.LangObjc    -> LangObjc
  Ext.LangObjcxx  -> LangObjcxx
  Ext.LangAsm     -> LangAsm
  Ext.RawObject   -> RawObject

---------------

data StgModule
  = StgModule
  { stgUnit         :: Unit
  , stgModuleName   :: ModuleName
  , stgModuleTyCons :: [TyCon]
  , stgTopBindings  :: [StgTopBinding]
  , stgForeignStubs :: ForeignStubs
  , stgForeignFiles :: [(ForeignSrcLang, FilePath)]
  , stgIdUniqueMap  :: [(Ext.Unique, Unique)]
  }

isAlgDataCon :: Ext.DataCon -> Bool
isAlgDataCon Ext.DataCon{..} = case dcRep of
  Ext.UnboxedTupleCon{} -> False
  _                     -> True

toStg :: Ext.Module -> StgModule
toStg Ext.Module{..} = stgModule where
  (topBindings, Env{..}) = flip runState (emptyEnv moduleUnitId moduleName) $ do
    setAlgTyCons algTyCons
    mapM cvtTopBinding moduleTopBindings

  algTyCons :: [Ext.TyCon]
  algTyCons =
    [ tc
    | tc <- concatMap (concatMap snd . snd) moduleTyCons
    , all isAlgDataCon $ Ext.tcDataCons tc
    ]

  localTyConIds :: Set Ext.TyConId
  localTyConIds = Set.fromList
    [ Ext.tcId tc
    | (uid, ml) <- moduleTyCons
    , uid == moduleUnitId
    , (mod, tcs) <- ml
    , mod == moduleName
    , tc <- tcs
    , all isAlgDataCon $ Ext.tcDataCons tc
    ]

  stgModule = StgModule
    { stgUnit         = cvtUnitId moduleUnitId
    , stgModuleName   = cvtModuleName moduleName
    , stgModuleTyCons = Map.elems $ Map.restrictKeys envADTTyConMap localTyConIds
    , stgTopBindings  = topBindings
    , stgForeignStubs = cvtForeignStubs moduleForeignStubs
    , stgForeignFiles = [(cvtForeignSrcLang s, f) | (s, f) <- moduleForeignFiles]
    , stgIdUniqueMap  = [(u, idUnique i) | (Ext.BinderId u, i) <- Map.toList envIdMap]
    }

{-
  = Module
  [no]    { modulePhase               :: !BS8.ByteString
  [maybe] , moduleUnitId              :: !UnitId
  [maybe] , moduleName                :: !ModuleName
  [yes]   , moduleForeignStubs        :: !ForeignStubs
  [no]    , moduleHasForeignExported  :: !Bool
  [no]    , moduleDependency          :: ![(UnitId, [ModuleName])]
  [no]    , moduleExternalTopIds      :: ![(UnitId, [(ModuleName, [idBnd])])]
  [yes]   , moduleTyCons              :: ![(UnitId, [(ModuleName, [TyCon' dcBnd])])]
  [no]    , moduleHaskellExported     :: ![(UnitId, [(ModuleName, [BinderId])])]
  [no]    , moduleForeignExported     :: ![(UnitId, [(ModuleName, [BinderId])])]
  [yes]   , moduleTopBindings         :: ![TopBinding' idBnd idOcc dcOcc]
  [yes]   , moduleForeignFiles        :: ![(ForeignSrcLang, FilePath)]
  [no]    , moduleCoreSrc             :: !BS8.ByteString
  [no]    , modulePrepCoreSrc         :: !BS8.ByteString
  }
-}
{-
  QUESTIONS:
    - how to build alg TyCons?
    - tag to enum primop needs type information ; how to build it?
    - do we need monad? i.e. assign new uniques, access data cons via Reader Env
  PROBLEMS:
    done - additional type info beside PrimRep in some cases
      done + AlgType
      done + ConApp ; OpApp

-}
