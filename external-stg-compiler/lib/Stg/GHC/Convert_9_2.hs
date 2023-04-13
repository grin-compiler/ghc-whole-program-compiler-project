{-# LANGUAGE ImplicitParams, RecordWildCards, LambdaCase, MultiWayIf, TupleSections, OverloadedStrings, CPP #-}

#ifdef EXTERNAL_STG_COMPILER_PACKAGE
module Stg.GHC.Convert_9_2 where
import Stg.Syntax
#else
module GHC.Stg.External.Convert where
import GHC.Stg.External.Syntax
import GHC.Prelude
#endif

import qualified Data.ByteString.Char8 as BS8

import qualified GHC.Hs.Decls           as GHC
import qualified GHC.Builtin.PrimOps    as GHC
import qualified GHC.Core               as GHC
import qualified GHC.Core.DataCon       as GHC
import qualified GHC.Core.TyCon         as GHC
import qualified GHC.Core.TyCo.Ppr      as GHC
import qualified GHC.Core.TyCo.Rep      as GHC
import qualified GHC.Core.Type          as GHC
import qualified GHC.Core.Utils         as GHC
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Driver.Session     as GHC
import qualified GHC.Driver.Ppr         as GHC
import qualified GHC.Stg.Syntax         as GHC
import qualified GHC.Types.Basic        as GHC
import qualified GHC.Types.ForeignCall  as GHC
import qualified GHC.Types.Id           as GHC
import qualified GHC.Types.Id.Info      as GHC
import qualified GHC.Types.Literal      as GHC
import qualified GHC.Types.Name         as GHC
import qualified GHC.Types.SrcLoc       as GHC
import qualified GHC.Types.RepType      as GHC
import qualified GHC.Types.Unique       as GHC
import qualified GHC.Types.Tickish      as GHC
import qualified GHC.Types.SourceText   as GHC
import qualified GHC.Types.ForeignStubs as GHC
--import qualified GHC.Types.ForeignStubDecls as GHC
import qualified GHC.Unit.Types         as GHC
import qualified GHC.Unit.Module.Name   as GHC
import qualified GHC.Utils.Outputable   as GHC

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace
import Control.Exception
import System.IO.Unsafe
import qualified GHC.Core.Ppr as GHC

--trace :: String -> a -> a
--trace _ = id

-- conversion env

data Env
  = Env
  { envExternalIds    :: IntMap GHC.Id
  , envTyCons         :: IntMap GHC.TyCon

  -- debug state
  , envDefinedUnique  :: IntSet

  , envNameSrcLoc     :: IntMap GHC.SrcSpan
  }

emptyEnv :: Env
emptyEnv = Env
  { envExternalIds    = IntMap.empty
  , envTyCons         = IntMap.empty
  , envDefinedUnique  = IntSet.empty
  , envNameSrcLoc     = IntMap.empty
  }

type M = State Env

-- debug

checkName :: (?dflags :: GHC.DynFlags) => GHC.Name -> M ()
checkName n = do
  let key = uniqueKey n
      loc = GHC.nameSrcSpan n
  lastLoc <- state $ \env@Env{..} -> (IntMap.lookup key envNameSrcLoc, env {envNameSrcLoc = IntMap.insert key loc envNameSrcLoc})
  case lastLoc of
    Nothing -> pure ()
    Just l  -> when (l == loc) $ do
      error $ ppr n ++ " has multiple source locations, previous: " ++ show l ++ " current: " ++ show loc

defKey :: (GHC.Uniquable a, GHC.Outputable a, ?dflags :: GHC.DynFlags) => a -> M ()
defKey a = do
  let key = uniqueKey a
  wasDefined <- state $ \env@Env{..} -> (IntSet.member key envDefinedUnique, env {envDefinedUnique = IntSet.insert key envDefinedUnique})
  when wasDefined $ do
    error $ "redefinition of: " ++ ppr a

-- helpers

ppr :: (GHC.Outputable a, ?dflags :: GHC.DynFlags) => a -> String
ppr = GHC.showSDoc ?dflags . GHC.ppr

showSDoc :: (?dflags :: GHC.DynFlags) => GHC.SDoc -> String
showSDoc = GHC.showSDoc ?dflags

bs8SDoc :: (?dflags :: GHC.DynFlags) => GHC.SDoc -> BS8.ByteString
bs8SDoc = BS8.pack . GHC.showSDoc ?dflags

uniqueKey :: GHC.Uniquable a => a -> Int
uniqueKey = GHC.getKey . GHC.getUnique

cvtUnique :: GHC.Unique -> Unique
cvtUnique u = Unique a b
  where (a,b) = GHC.unpkUnique u

-- name conversion

cvtOccName :: GHC.OccName -> Name
cvtOccName = GHC.bytesFS . GHC.occNameFS

cvtUnitId :: GHC.Unit -> UnitId
cvtUnitId = UnitId . GHC.bytesFS . GHC.unitIdFS . GHC.toUnitId

cvtModuleName :: GHC.ModuleName -> ModuleName
cvtModuleName = ModuleName . GHC.bytesFS . GHC.moduleNameFS

cvtUnitIdAndModuleName :: GHC.Module -> (UnitId, ModuleName)
cvtUnitIdAndModuleName m = (cvtUnitId $ GHC.moduleUnit m, cvtModuleName $ GHC.moduleName m)

-- source location conversion

cvtBufSpan :: GHC.BufSpan -> BufSpan
cvtBufSpan (GHC.BufSpan s e) = BufSpan (GHC.bufPos s) (GHC.bufPos e)

cvtRealSrcSpan :: GHC.RealSrcSpan -> RealSrcSpan
cvtRealSrcSpan rs =
  RealSrcSpan'
  { srcSpanFile   = GHC.bytesFS $ GHC.srcSpanFile rs
  , srcSpanSLine  = GHC.srcSpanStartLine rs
  , srcSpanSCol   = GHC.srcSpanStartCol rs
  , srcSpanELine  = GHC.srcSpanEndLine rs
  , srcSpanECol   = GHC.srcSpanEndCol rs
  }

cvtSrcSpan :: GHC.SrcSpan -> SrcSpan
cvtSrcSpan = \case
  GHC.RealSrcSpan s mb  -> RealSrcSpan (cvtRealSrcSpan s) (fmap cvtBufSpan mb)
  GHC.UnhelpfulSpan r   -> UnhelpfulSpan $ cvtUnhelpfulSpanReason r

cvtUnhelpfulSpanReason :: GHC.UnhelpfulSpanReason -> UnhelpfulSpanReason
cvtUnhelpfulSpanReason = \case
  GHC.UnhelpfulNoLocationInfo -> UnhelpfulNoLocationInfo
  GHC.UnhelpfulWiredIn        -> UnhelpfulWiredIn
  GHC.UnhelpfulInteractive    -> UnhelpfulInteractive
  GHC.UnhelpfulGenerated      -> UnhelpfulGenerated
  GHC.UnhelpfulOther s        -> UnhelpfulOther $ GHC.bytesFS s

-- tickish conversion

cvtTickish :: GHC.StgTickish -> Tickish
cvtTickish = \case
  GHC.ProfNote{}      -> ProfNote
  GHC.HpcTick{}       -> HpcTick
  GHC.Breakpoint{}    -> Breakpoint
  GHC.SourceNote{..}  -> SourceNote (cvtRealSrcSpan sourceSpan) (BS8.pack sourceName)

-- data con conversion

addTyCon :: GHC.TyCon -> M ()
addTyCon tc = do
  let tcKey = uniqueKey tc
  new <- IntMap.notMember tcKey <$> gets envTyCons
  when new $ modify' $ \m@Env{..} -> m {envTyCons = IntMap.insert tcKey tc envTyCons}

cvtDataCon :: GHC.DataCon -> M DataConId
cvtDataCon dc = do
  addTyCon $ GHC.dataConTyCon dc
  pure . DataConId . cvtUnique . GHC.getUnique $ dc

-- type conversion

cvtDataTyConIdFromType :: (?dflags :: GHC.DynFlags) => GHC.Type -> M (Maybe TyConId)
cvtDataTyConIdFromType t
  | Just tc <- GHC.tyConAppTyConPicky_maybe t
  , GHC.isDataTyCon tc
  = Just <$> cvtDataTyConId tc

  | otherwise
  = pure Nothing

cvtDataTyConId :: (?dflags :: GHC.DynFlags) => GHC.TyCon -> M TyConId
cvtDataTyConId tc
  | GHC.isDataTyCon tc
  = do
    addTyCon tc
    pure . TyConId . cvtUnique . GHC.getUnique $ tc
  | otherwise
  = error $ "cvtDataTyConId: not DataTyCon; " ++ ppr tc

instance GHC.Outputable Type where
  ppr = GHC.text . show

trpp :: (GHC.Outputable o, GHC.Outputable a, ?dflags :: GHC.DynFlags) => String -> (o -> a) -> o -> a
trpp msg f a = trace (unwords [msg, ":"]) $
               trace ('\t' : ppr a) $
               trace (unwords ["\t\t=", ppr (f a), "\n-----------\n"]) $
               f a

{-# INLINE debugCvtAppType #-}
debugCvtAppType :: (?dflags :: GHC.DynFlags) => GHC.Id -> [GHC.StgArg] -> GHC.Type -> String -> Type
debugCvtAppType f args ty msg = unsafePerformIO $ debugCvtAppTypeM f args ty msg

{-# INLINE debugCvtAppTypeM #-}
debugCvtAppTypeM :: (?dflags :: GHC.DynFlags) => GHC.Id -> [GHC.StgArg] -> GHC.Type -> String -> IO Type
debugCvtAppTypeM f args ty msg = catch (let t = cvtTypeNormal ty in seq t (pure t)) $ \ex -> do
  putStrLn $ "cought exception during StgApp result type conversion"
  putStrLn "Normal:"
  putStrLn "function:"
  putStrLn $ "  " ++ ppr f ++ " :: " ++ ppr (GHC.idType f)
  putStrLn "args:"
  forM_ args $ \a -> case a of
    GHC.StgVarArg o -> putStrLn $ "    " ++ ppr o ++ " :: " ++ ppr (GHC.idType o)
    GHC.StgLitArg l -> putStrLn $ "    " ++ ppr l ++ " :: " ++ ppr (GHC.literalType l)
  putStrLn $ "function result type:"
  putStrLn $ "  " ++ ppr ty

  putStrLn "Debug:"
  putStrLn "function:"
  putStrLn $ "  " ++ ppr f ++ " :: " ++ showSDoc (GHC.debugPprType $ GHC.idType f)
  putStrLn "args:"
  forM_ args $ \a -> case a of
    GHC.StgVarArg o -> putStrLn $ "    " ++ ppr o ++ " :: " ++ showSDoc (GHC.debugPprType $ GHC.idType o)
    GHC.StgLitArg l -> putStrLn $ "    " ++ ppr l ++ " :: " ++ showSDoc (GHC.debugPprType $ GHC.literalType l)
  putStrLn $ "function result type:"
  putStrLn $ "  " ++ showSDoc (GHC.debugPprType ty)
  putStrLn $ "StgApp type label:"
  putStrLn $ "  " ++ msg
  throwIO (ex :: SomeException)

cvtType :: (?dflags :: GHC.DynFlags) => String -> GHC.Type -> Type
--cvtType msg t = trpp (unwords [msg, "cvtType"]) cvtType3 $ deepCheckType t
cvtType _ = cvtTypeNormal

deepCheckType :: GHC.Type -> GHC.Type
deepCheckType t = t -- TODO
{-
cvtType3 :: GHC.Type -> Type
cvtType3 t
  | trpp "  isTypeLevPoly" GHC.isTypeLevPoly t
  = PolymorphicRep

  | trpp "  isUnboxedTupleType" GHC.isUnboxedTupleType t
  = UnboxedTuple []

  | otherwise
  = SingleValue LiftedRep
-}
{-
cvtType msg t = trace
  ( "cvtType - " ++ msg ++
    " isPiTy: " ++ show (GHC.isPiTy t) ++
    " isDictLikeTy: " ++ (show $ GHC.isDictLikeTy t) ++
    " isTypeLevPoly: " ++ (show $ GHC.isTypeLevPoly t) ++
    " isUnboxedTupleType: " ++ (show $ GHC.isUnboxedTupleType t) ++
    " isFunTy: " ++ (show $ GHC.isFunTy t) ++
    " :: " ++ ppr t
  ) . cvtType2 $ t
-}
{-
isDictLikeTy :: Type -> Bool
-- Note [Dictionary-like types]

-- | Returns Just True if this type is surely lifted, Just False
-- if it is surely unlifted, Nothing if we can't be sure (i.e., it is
-- levity polymorphic), and panics if the kind does not have the shape
-- TYPE r.
isLiftedType_maybe :: HasDebugCallStack => Type -> Maybe Bool
-}

{-# INLINE cvtTypeNormal #-}
cvtTypeNormal :: (?dflags :: GHC.DynFlags) => GHC.Type -> Type
cvtTypeNormal t
  | GHC.isTypeLevPoly t
  = PolymorphicRep

  | GHC.isUnboxedSumType t || GHC.isUnboxedTupleType t
  = UnboxedTuple (map cvtPrimRep $ GHC.typePrimRep t)

  | [rep] <- GHC.typePrimRepArgs t
  = SingleValue (cvtPrimRep rep)

  | otherwise
  = error $ "could not convert type: " ++ ppr t

cvtPrimRep :: GHC.PrimRep -> PrimRep
cvtPrimRep = \case
  GHC.VoidRep     -> VoidRep
  GHC.LiftedRep   -> LiftedRep
  GHC.UnliftedRep -> UnliftedRep
  GHC.Int8Rep     -> Int8Rep
  GHC.Int16Rep    -> Int16Rep
  GHC.Int32Rep    -> Int32Rep
  GHC.Int64Rep    -> Int64Rep
  GHC.IntRep      -> IntRep
  GHC.Word8Rep    -> Word8Rep
  GHC.Word16Rep   -> Word16Rep
  GHC.Word32Rep   -> Word32Rep
  GHC.Word64Rep   -> Word64Rep
  GHC.WordRep     -> WordRep
  GHC.AddrRep     -> AddrRep
  GHC.FloatRep    -> FloatRep
  GHC.DoubleRep   -> DoubleRep
  GHC.VecRep i e  -> VecRep i $ cvtPrimElemRep e

cvtPrimElemRep :: GHC.PrimElemRep -> PrimElemRep
cvtPrimElemRep = \case
  GHC.Int8ElemRep   -> Int8ElemRep
  GHC.Int16ElemRep  -> Int16ElemRep
  GHC.Int32ElemRep  -> Int32ElemRep
  GHC.Int64ElemRep  -> Int64ElemRep
  GHC.Word8ElemRep  -> Word8ElemRep
  GHC.Word16ElemRep -> Word16ElemRep
  GHC.Word32ElemRep -> Word32ElemRep
  GHC.Word64ElemRep -> Word64ElemRep
  GHC.FloatElemRep  -> FloatElemRep
  GHC.DoubleElemRep -> DoubleElemRep

-- literal conversion

cvtLitNumType :: GHC.LitNumType -> LitNumType
cvtLitNumType = \case
  GHC.LitNumInt     -> LitNumInt
  GHC.LitNumInt8    -> LitNumInt8
  GHC.LitNumInt16   -> LitNumInt16
  GHC.LitNumInt32   -> LitNumInt32
  GHC.LitNumInt64   -> LitNumInt64
  GHC.LitNumWord    -> LitNumWord
  GHC.LitNumWord8   -> LitNumWord8
  GHC.LitNumWord16  -> LitNumWord16
  GHC.LitNumWord32  -> LitNumWord32
  GHC.LitNumWord64  -> LitNumWord64

cvtLabelSpec :: Maybe Int -> GHC.FunctionOrData -> LabelSpec
cvtLabelSpec mi = \case
  GHC.IsFunction  -> FunctionLabel mi
  GHC.IsData      -> DataLabel

cvtLit :: GHC.Literal -> Lit
cvtLit = \case
  GHC.LitChar x       -> LitChar x
  GHC.LitString x     -> LitString x
  GHC.LitNullAddr     -> LitNullAddr
  GHC.LitFloat x      -> LitFloat x
  GHC.LitDouble x     -> LitDouble x
  GHC.LitLabel x i d  -> LitLabel (GHC.bytesFS  x) (cvtLabelSpec i d)
  GHC.LitNumber t i   -> LitNumber (cvtLitNumType t) i

-- Id conversion

mkBinderId :: GHC.Uniquable a => a -> BinderId
mkBinderId = BinderId . cvtUnique . GHC.getUnique

cvtOccId :: GHC.Id -> M BinderId
cvtOccId x = do
  let name = GHC.getName x
  when (GHC.isExternalName name) $ do
    let key = uniqueKey x
    new <- IntMap.notMember key <$> gets envExternalIds
    when new $ modify' $ \m@Env{..} -> m {envExternalIds = IntMap.insert key x envExternalIds}
  --checkName name
  pure $ mkBinderId x

isForeignExportedId :: GHC.Id -> Bool
isForeignExportedId i = case GHC.idDetails i of
#ifndef EXT_STG_FOR_NON_PATCHED_GHC
  GHC.FExportedId -> True
#endif
  _               -> False

cvtIdDetails :: GHC.Id -> M IdDetails
cvtIdDetails i = case GHC.idDetails i of
  GHC.VanillaId       -> pure VanillaId
#ifndef EXT_STG_FOR_NON_PATCHED_GHC
  GHC.FExportedId     -> pure FExportedId
#endif
  GHC.RecSelId{}      -> pure RecSelId
  GHC.DataConWorkId d -> DataConWorkId <$> cvtDataCon d
  GHC.DataConWrapId d -> DataConWrapId <$> cvtDataCon d
  GHC.ClassOpId{}     -> pure ClassOpId
  GHC.PrimOpId{}      -> pure PrimOpId
  GHC.FCallId{}       -> pure FCallId
  GHC.TickBoxOpId{}   -> pure TickBoxOpId
  GHC.DFunId{}        -> pure DFunId
  GHC.CoVarId{}       -> pure CoVarId
  GHC.JoinId ar       -> pure $ JoinId ar Nothing

cvtScope :: GHC.Id -> Scope
cvtScope i
  | GHC.isGlobalId i    = if isForeignExportedId i then ForeignExported else HaskellExported
  | GHC.isExportedId i  = GlobalScope -- HINT: top level local
  | otherwise           = LocalScope

cvtBinderIdClosureParam :: (?dflags :: GHC.DynFlags) => IdDetails -> String -> GHC.Id -> SBinder
cvtBinderIdClosureParam details msg v
  | GHC.isId v = SBinder
      { sbinderName     = cvtOccName $ GHC.getOccName v
      , sbinderId       = BinderId . cvtUnique . GHC.idUnique $ v
      , sbinderType     = SingleValue . cvtPrimRep . {-trpp (unwords [msg, "cvtBinderIdClosureParam", ppr v])-} GHC.typePrimRep1 $ GHC.idType v
      , sbinderTypeSig  = BS8.pack . ppr $ GHC.idType v
      , sbinderScope    = cvtScope v
      , sbinderDetails  = details
      , sbinderInfo     = BS8.pack . ppr $ GHC.idInfo v
      , sbinderDefLoc   = cvtSrcSpan . GHC.nameSrcSpan $ GHC.getName v
      }
  | otherwise = error $ "Type binder in STG: " ++ (show $ cvtOccName $ GHC.getOccName v)


cvtBinderId :: (?dflags :: GHC.DynFlags) => IdDetails -> String -> GHC.Id -> SBinder
cvtBinderId details msg v
  | GHC.isId v = SBinder
      { sbinderName     = cvtOccName $ GHC.getOccName v
      , sbinderId       = BinderId . cvtUnique . GHC.idUnique $ v
      , sbinderType     = cvtType (unwords [msg, "cvtBinderId", ppr v]) $ GHC.idType v
      , sbinderTypeSig  = BS8.pack . ppr $ GHC.idType v
      , sbinderScope    = cvtScope v
      , sbinderDetails  = details
      , sbinderInfo     = BS8.pack . ppr $ GHC.idInfo v
      , sbinderDefLoc   = cvtSrcSpan . GHC.nameSrcSpan $ GHC.getName v
      }
  | otherwise = error $ "Type binder in STG: " ++ (show $ cvtOccName $ GHC.getOccName v)

cvtBinderIdClosureParamM :: (?dflags :: GHC.DynFlags) => String -> GHC.Id -> M SBinder
cvtBinderIdClosureParamM msg i = do
  --defKey i -- debug
  details <- cvtIdDetails i
  pure $ cvtBinderIdClosureParam details msg i

cvtBinderIdM :: (?dflags :: GHC.DynFlags) => String -> GHC.Id -> M SBinder
cvtBinderIdM msg i = do
  --checkName $ GHC.getName i
  --defKey i -- debug
  details <- cvtIdDetails i
  pure $ cvtBinderId details msg i

-- stg op conversion

cvtSourceText :: GHC.SourceText -> SourceText
cvtSourceText = \case
  GHC.SourceText s  -> SourceText (BS8.pack s)
  GHC.NoSourceText  -> NoSourceText

cvtCCallTarget :: GHC.CCallTarget -> CCallTarget
cvtCCallTarget = \case
  GHC.StaticTarget s l u b  -> StaticTarget (cvtSourceText s) (GHC.bytesFS l) (fmap cvtUnitId u) b
  GHC.DynamicTarget         -> DynamicTarget

cvtCCallConv :: GHC.CCallConv -> CCallConv
cvtCCallConv = \case
  GHC.CCallConv           -> CCallConv
  GHC.CApiConv            -> CApiConv
  GHC.StdCallConv         -> StdCallConv
  GHC.PrimCallConv        -> PrimCallConv
  GHC.JavaScriptCallConv  -> JavaScriptCallConv

cvtSafety :: GHC.Safety -> Safety
cvtSafety = \case
  GHC.PlaySafe          -> PlaySafe
  GHC.PlayInterruptible -> PlayInterruptible
  GHC.PlayRisky         -> PlayRisky

cvtForeignCall :: GHC.ForeignCall -> ForeignCall
cvtForeignCall (GHC.CCall (GHC.CCallSpec t c s)) = ForeignCall (cvtCCallTarget t) (cvtCCallConv c) (cvtSafety s)

cvtPrimCall :: GHC.PrimCall -> PrimCall
cvtPrimCall (GHC.PrimCall lbl uid) = PrimCall (GHC.bytesFS lbl) (cvtUnitId uid)

cvtOp :: GHC.StgOp -> StgOp
cvtOp = \case
  GHC.StgPrimOp o     -> StgPrimOp (cvtOccName $ GHC.primOpOcc o)
  GHC.StgPrimCallOp p -> StgPrimCallOp $ cvtPrimCall p
  GHC.StgFCallOp f _  -> StgFCallOp $ cvtForeignCall f

-- arg conversion

cvtArg :: GHC.StgArg -> M SArg
cvtArg = \case
  GHC.StgVarArg o -> StgVarArg <$> cvtOccId o
  GHC.StgLitArg l -> pure $ StgLitArg (cvtLit l)

-- alt conversion

cvtAltType :: (?dflags :: GHC.DynFlags) => GHC.AltType -> M SAltType
cvtAltType = \case
  GHC.PolyAlt       -> pure $ PolyAlt
  GHC.MultiValAlt i -> pure $ MultiValAlt i
  GHC.PrimAlt r     -> pure $ PrimAlt $ cvtPrimRep r
  GHC.AlgAlt tc     -> AlgAlt <$> cvtDataTyConId tc

cvtAlt :: (?dflags :: GHC.DynFlags) => GHC.StgAlt -> M SAlt
cvtAlt (con, bs, e) = Alt <$> cvtAltCon con <*> mapM (cvtBinderIdM "Alt") bs <*> cvtExpr e

cvtAltCon :: GHC.AltCon -> M SAltCon
cvtAltCon = \case
  GHC.DataAlt con -> AltDataCon <$> cvtDataCon con
  GHC.LitAlt l    -> pure . AltLit $ cvtLit l
  GHC.DEFAULT     -> pure $ AltDefault

-- stg expr conversion

cvtExpr :: (?dflags :: GHC.DynFlags) => GHC.StgExpr -> M SExpr
cvtExpr = \case
#ifdef EXT_STG_FOR_NON_PATCHED_GHC
  GHC.StgApp f ps           -> StgApp <$> cvtOccId f <*> mapM cvtArg ps <*> pure PolymorphicRep <*> pure mempty
#else
  GHC.StgApp f ps tr        -> StgApp <$> cvtOccId f <*> mapM cvtArg ps <*> pure ({-cvtType "StgApp" t-}debugCvtAppType f ps tr "") <*> pure (BS8.pack . ppr $ GHC.idType f, BS8.pack $ ppr tr, BS8.pack "")
#endif
  GHC.StgLit l              -> pure $ StgLit (cvtLit l)
  GHC.StgConApp dc _ ps ts  -> StgConApp <$> cvtDataCon dc <*> mapM cvtArg ps <*> pure (map (cvtType "StgConApp") ts)
  GHC.StgOpApp o ps t       -> StgOpApp (cvtOp o) <$> mapM cvtArg ps <*> pure (cvtType "StgOpApp" t) <*> cvtDataTyConIdFromType t
  GHC.StgCase e b at al     -> StgCase <$> cvtExpr e <*> cvtBinderIdM "StgCase" b <*> cvtAltType at <*> mapM cvtAlt al
  GHC.StgLet _ b e          -> StgLet <$> cvtBind b <*> cvtExpr e
  GHC.StgLetNoEscape _ b e  -> StgLetNoEscape <$> cvtBind b <*> cvtExpr e
  GHC.StgTick t e           -> StgTick (cvtTickish t) <$> cvtExpr e
  e                         -> error $ "invalid stg expression: " ++ ppr (GHC.pprStgExpr GHC.panicStgPprOpts e)

-- stg rhs conversion (heap objects)

cvtUpdateFlag :: GHC.UpdateFlag -> UpdateFlag
cvtUpdateFlag = \case
  GHC.ReEntrant   -> ReEntrant
  GHC.Updatable   -> Updatable
  GHC.SingleEntry -> SingleEntry

cvtRhs :: (?dflags :: GHC.DynFlags) => GHC.StgRhs -> M SRhs
cvtRhs = \case
  GHC.StgRhsClosure _ _ u bs e  -> StgRhsClosure [] (cvtUpdateFlag u) <$> mapM (cvtBinderIdClosureParamM "StgRhsClosure") bs <*> cvtExpr e
  GHC.StgRhsCon _ dc _ _ args   -> StgRhsCon <$> cvtDataCon dc <*> mapM cvtArg args

-- bind and top-bind conversion

cvtBind :: (?dflags :: GHC.DynFlags) => GHC.StgBinding -> M SBinding
cvtBind = \case
  GHC.StgNonRec b r -> StgNonRec <$> cvtBinderIdM "StgNonRec" b <*> cvtRhs r
  GHC.StgRec    bs  -> StgRec <$> sequence [(,) <$> cvtBinderIdM "StgRec" b <*> cvtRhs r | (b, r) <- bs]

cvtTopBind :: (?dflags :: GHC.DynFlags) => GHC.StgTopBinding -> M STopBinding
cvtTopBind = \case
  GHC.StgTopLifted b        -> StgTopLifted <$> cvtBind b
  GHC.StgTopStringLit b bs  -> StgTopStringLit <$> cvtBinderIdM "StgTopStringLit" b <*> pure bs

cvtTopBindsAndStubs :: (?dflags :: GHC.DynFlags) => [GHC.StgTopBinding] -> GHC.ForeignStubs -> GHC.ForeignStubDecls -> M ([STopBinding], SForeignStubs, [(UnitId, [(ModuleName, [SBinder])])])
cvtTopBindsAndStubs binds stubs decls = do
  b <- mapM cvtTopBind binds
  s <- cvtForeignStubs stubs decls

  let stgTopIds = concatMap topBindIds binds
      topKeys   = IntSet.fromList $ map uniqueKey stgTopIds
  Env{..} <- get
  extItems <- sequence [mkExternalName e | (k,e) <- IntMap.toList envExternalIds, IntSet.notMember k topKeys]
  pure (b, s, groupByUnitIdAndModule extItems)

-- foreign stubs

cvtHeader :: GHC.Header -> Header
cvtHeader (GHC.Header t n) = Header (cvtSourceText t) (GHC.bytesFS n)

cvtCImportSpec :: GHC.CImportSpec -> CImportSpec
cvtCImportSpec = \case
  GHC.CLabel n    -> CLabel $ GHC.bytesFS n
  GHC.CFunction t -> CFunction $ cvtCCallTarget t
  GHC.CWrapper    -> CWrapper

cvtCExportSpec :: GHC.CExportSpec -> CExportSpec
cvtCExportSpec (GHC.CExportStatic t n cc) = CExportStatic (cvtSourceText t) (GHC.bytesFS n) (cvtCCallConv cc)

cvtStubImpl :: (?dflags :: GHC.DynFlags) => GHC.StubImpl -> StubImpl
cvtStubImpl = \case
  GHC.StubImplImportCWrapper n m  -> StubImplImportCWrapper (GHC.bytesFS n) m
  GHC.StubImplImportCApi n l      -> StubImplImportCApi (GHC.bytesFS n) [(fmap cvtHeader h, bs8SDoc d, c) | (h, d, c) <- l]

cvtForeignImport :: GHC.ForeignImport -> ForeignImport
cvtForeignImport (GHC.CImport cc s m is t) = CImport (cvtCCallConv $ GHC.unLoc cc) (cvtSafety $ GHC.unLoc s) (fmap cvtHeader m) (cvtCImportSpec is) (cvtSourceText $ GHC.unLoc t)

cvtForeignExport :: GHC.ForeignExport -> ForeignExport
cvtForeignExport (GHC.CExport s t) = CExport (cvtCExportSpec $ GHC.unLoc s) (cvtSourceText $ GHC.unLoc t)

cvtStubDecl :: (?dflags :: GHC.DynFlags) => GHC.StubDecl -> M SStubDecl
cvtStubDecl = \case
  GHC.StubDeclImport fi m -> pure $ StubDeclImport (cvtForeignImport fi) (fmap cvtStubImpl m)
  GHC.StubDeclExport fe idOcc n -> StubDeclExport (cvtForeignExport fe) <$> cvtOccId idOcc <*> pure (BS8.pack n)

cvtForeignStubs :: (?dflags :: GHC.DynFlags) => GHC.ForeignStubs -> GHC.ForeignStubDecls -> M SForeignStubs
cvtForeignStubs stubs (GHC.ForeignStubDecls decls) = case stubs of
  GHC.NoStubs
    -> pure NoStubs
  GHC.ForeignStubs (GHC.CHeader h) (GHC.CStub c)
    -> ForeignStubs (bs8SDoc $ GHC.pprCode GHC.CStyle h) (bs8SDoc $ GHC.pprCode GHC.CStyle c) [] [] <$> mapM cvtStubDecl [d | (_, _, d) <- decls]

cvtForeignSrcLang :: GHC.ForeignSrcLang -> ForeignSrcLang
cvtForeignSrcLang = \case
  GHC.LangC       -> LangC
  GHC.LangCxx     -> LangCxx
  GHC.LangObjc    -> LangObjc
  GHC.LangObjcxx  -> LangObjcxx
  GHC.LangAsm     -> LangAsm
  GHC.RawObject   -> RawObject

-- module conversion

cvtModule :: GHC.DynFlags -> String -> GHC.Unit -> GHC.ModuleName -> Maybe FilePath -> [GHC.StgTopBinding] -> GHC.ForeignStubs -> GHC.ForeignStubDecls -> [(GHC.ForeignSrcLang, FilePath)] -> SModule
cvtModule dflags phase unit' modName' mSrcPath binds foreignStubs foreignDecls foreignFiles = let ?dflags = dflags in cvtModule' phase unit' modName' mSrcPath binds foreignStubs foreignDecls foreignFiles

cvtModule' :: (?dflags :: GHC.DynFlags) => String -> GHC.Unit -> GHC.ModuleName -> Maybe FilePath -> [GHC.StgTopBinding] -> GHC.ForeignStubs -> GHC.ForeignStubDecls -> [(GHC.ForeignSrcLang, FilePath)] -> SModule
cvtModule' phase unit' modName' mSrcPath binds foreignStubs foreignDecls foreignFiles =
  Module
  { modulePhase               = BS8.pack phase
  , moduleUnitId              = unitId
  , moduleName                = modName
  , moduleSourceFilePath      = fmap BS8.pack mSrcPath
  , moduleForeignStubs        = stubs
  , moduleHasForeignExported  = any isForeignExportedId stgTopIds
  , moduleDependency          = dependencies
  , moduleExternalTopIds      = externalIds
  , moduleTyCons              = tyCons
  , moduleTopBindings         = topBinds
  , moduleForeignFiles        = [(cvtForeignSrcLang s, p) | (s, p) <- foreignFiles]
  } where
      -- ?dflags             = dflags
      ((topBinds, stubs, externalIds), Env{..}) = runState (cvtTopBindsAndStubs binds foreignStubs foreignDecls) initialEnv

      initialEnv          = emptyEnv
      stgTopIds           = concatMap topBindIds binds
      modName             = cvtModuleName modName'
      unitId              = cvtUnitId unit'
      tyCons              = groupByUnitIdAndModule . map mkTyCon $ IntMap.elems envTyCons

      -- calculate dependencies
      externalTyCons      = [(cvtUnitIdAndModuleName m, ()) | m <- catMaybes $ map (GHC.nameModule_maybe . GHC.getName) $ IntMap.elems envTyCons]
      dependencies        = map (fmap (map fst)) $ groupByUnitIdAndModule $ [((u, m), ()) | (u, ml) <- externalIds, (m, _) <- ml] ++ externalTyCons

-- utils

groupByUnitIdAndModule :: Ord b => [((UnitId, ModuleName), b)] -> [(UnitId, [(ModuleName, [b])])]
groupByUnitIdAndModule l =
  Map.toList . fmap (Map.toList . fmap Set.toList) $
  Map.unionsWith (Map.unionWith Set.union)
  [Map.singleton u (Map.singleton m (Set.singleton b)) | ((u, m), b) <- l]

mkExternalName :: (?dflags :: GHC.DynFlags) => GHC.Id -> M ((UnitId, ModuleName), SBinder)
mkExternalName x = (cvtUnitIdAndModuleName . GHC.nameModule $ GHC.getName x,) <$> cvtBinderIdM "mkExternalName" x

mkTyCon :: (?dflags :: GHC.DynFlags) => GHC.TyCon -> ((UnitId, ModuleName), STyCon)
mkTyCon tc = (cvtUnitIdAndModuleName $ GHC.nameModule n, b) where
  n = GHC.getName tc
  b = STyCon
      { stcName     = cvtOccName $ GHC.getOccName n
      , stcId       = TyConId . cvtUnique . GHC.getUnique $ n
      , stcDataCons = map mkSDataCon . sortDataCons $ GHC.tyConDataCons tc
      , stcDefLoc   = cvtSrcSpan $ GHC.nameSrcSpan n
      }
  sortDataCons l = IntMap.elems $ IntMap.fromList [(GHC.dataConTag dc, dc) | dc <- l]

mkSDataCon :: (?dflags :: GHC.DynFlags) => GHC.DataCon -> SDataCon
mkSDataCon dc = SDataCon
  { sdcName   = cvtOccName $ GHC.getOccName n
  , sdcId     = dataConId
  , sdcRep    = if
      | GHC.isUnboxedSumDataCon dc    -> error "unboxed sum cons are not supported in STG!"
      | GHC.isUnboxedTupleDataCon dc  -> UnboxedTupleCon $ GHC.dataConRepArity dc
      | otherwise                     -> AlgDataCon $ concatMap (concatMap getConArgRep . dcpp "3" GHC.typePrimRep . GHC.scaledThing) $ dcpp "2" GHC.dataConRepArgTys $ dcpp "1" id $ dc
  , sdcWorker = cvtBinderId idDetails "dataConWorkId" workerId
  , sdcDefLoc = cvtSrcSpan $ GHC.nameSrcSpan n
  } where
      dataConId = DataConId . cvtUnique . GHC.getUnique $ n
      workerId  = GHC.dataConWorkId dc

      idDetails = case GHC.idDetails workerId of
        GHC.DataConWorkId d
          | GHC.getUnique d == GHC.getUnique dc
          -> DataConWorkId dataConId
        _ -> error $ "invalid IdDetails for DataCon worker id: " ++ ppr (dc, workerId)

      dcpp :: GHC.Outputable o => String -> (o -> a) -> o -> a
      dcpp _ f x = f x
      --dcpp msg f a = trace ("mkSDataCon " ++ msg ++ " : " ++ ppr a) $ f a
      n = GHC.getName dc
      getConArgRep = \case
        GHC.VoidRep -> [] -- HINT: drop VoidRep arguments, the STG constructor builder code also ignores them
        r           -> [cvtPrimRep r]

topBindIds :: GHC.StgTopBinding -> [GHC.Id]
topBindIds = \case
  GHC.StgTopLifted (GHC.StgNonRec b _)  -> [b]
  GHC.StgTopLifted (GHC.StgRec bs)      ->  map fst bs
  GHC.StgTopStringLit b _               -> [b]
