{-# LANGUAGE ImplicitParams, RecordWildCards, LambdaCase, MultiWayIf, TupleSections, OverloadedStrings #-}
module WPC.StgToExtStg where

import Stg.Syntax

import GHC.Prelude

import qualified Data.ByteString.Char8 as BS8

import qualified GHC
import qualified GHC.Hs.Extension       as GHC
import qualified GHC.Hs.Decls           as GHC
import qualified GHC.Builtin.PrimOps    as GHC
import qualified GHC.Core               as GHC
import qualified GHC.Core.DataCon       as GHC
import qualified GHC.Core.TyCon         as GHC
import qualified GHC.Core.TyCo.Ppr      as GHC
import qualified GHC.Core.TyCo.Rep      as GHC
import qualified GHC.Core.Type          as GHC
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Driver.Session     as GHC
import qualified GHC.Driver.Ppr         as GHC
import qualified GHC.Stg.Syntax         as GHC
import qualified GHC.Cmm.CLabel         as GHC
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
--import qualified GHC.Utils.Panic.Plain  as GHC
import qualified GHC.Unit.Types         as GHC
--import qualified GHC.Unit.Module.Name as GHC
import qualified GHC.Utils.Outputable   as GHC
import qualified GHC.Data.Strict        as GHC
import Control.Monad
import Control.Monad.State.Strict
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Maybe
import Data.List
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace
import Control.Exception
import System.IO.Unsafe

import qualified WPC.ForeignStubDecls as WPC

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

data ImplicitEnv
  = ImplicitEnv
  { ieDflags :: GHC.DynFlags
  }

type M = State Env

-- debug

checkName :: (?ienv :: ImplicitEnv) => GHC.Name -> M ()
checkName n = do
  let key = uniqueKey n
      loc = GHC.nameSrcSpan n
  lastLoc <- state $ \env@Env{..} -> (IntMap.lookup key envNameSrcLoc, env {envNameSrcLoc = IntMap.insert key loc envNameSrcLoc})
  case lastLoc of
    Nothing -> pure ()
    Just l  -> when (l == loc) $ do
      error $ ppr n ++ " has multiple source locations, previous: " ++ show l ++ " current: " ++ show loc

defKey :: (GHC.Uniquable a, GHC.Outputable a, ?ienv :: ImplicitEnv) => a -> M ()
defKey a = do
  let key = uniqueKey a
  wasDefined <- state $ \env@Env{..} -> (IntSet.member key envDefinedUnique, env {envDefinedUnique = IntSet.insert key envDefinedUnique})
  when wasDefined $ do
    error $ "redefinition of: " ++ ppr a

-- helpers

ppr :: (GHC.Outputable a, ?ienv :: ImplicitEnv) => a -> String
ppr = GHC.showSDoc (ieDflags ?ienv) . GHC.ppr

showSDoc :: (?ienv :: ImplicitEnv) => GHC.SDoc -> String
showSDoc = GHC.showSDoc (ieDflags ?ienv)

bs8SDoc :: (?ienv :: ImplicitEnv) => GHC.SDoc -> BS8.ByteString
bs8SDoc = BS8.pack . GHC.showSDoc (ieDflags ?ienv)

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

cvtStrictMaybe :: GHC.Maybe a -> Maybe a
cvtStrictMaybe = \case
  GHC.Nothing -> Nothing
  GHC.Just a  -> Just a

cvtSrcSpan :: GHC.SrcSpan -> SrcSpan
cvtSrcSpan = \case
  GHC.RealSrcSpan s mb  -> RealSrcSpan (cvtRealSrcSpan s) (fmap cvtBufSpan $ cvtStrictMaybe mb)
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

cvtDataTyConIdFromType :: (?ienv :: ImplicitEnv) => GHC.Type -> M (Maybe TyConId)
cvtDataTyConIdFromType t
  | Just tc <- GHC.tyConAppTyConPicky_maybe t
  , GHC.isDataTyCon tc
  = Just <$> cvtDataTyConId tc

  | otherwise
  = pure Nothing

cvtDataTyConId :: (?ienv :: ImplicitEnv) => GHC.TyCon -> M TyConId
cvtDataTyConId tc
  | GHC.isDataTyCon tc
  = do
    addTyCon tc
    pure . TyConId . cvtUnique . GHC.getUnique $ tc
  | otherwise
  = error $ "cvtDataTyConId: not DataTyCon; " ++ ppr tc

instance GHC.Outputable Type where
  ppr = GHC.text . show

trpp :: (GHC.Outputable o, GHC.Outputable a, ?ienv :: ImplicitEnv) => String -> (o -> a) -> o -> a
trpp msg f a = trace (unwords [msg, ":"]) $
               trace ('\t' : ppr a) $
               trace (unwords ["\t\t=", ppr (f a), "\n-----------\n"]) $
               f a

{-# INLINE debugCvtAppType #-}
debugCvtAppType :: (?ienv :: ImplicitEnv) => GHC.Id -> [GHC.StgArg] -> GHC.Type -> String -> Type
debugCvtAppType f args ty msg = unsafePerformIO $ debugCvtAppTypeM f args ty msg

{-# INLINE debugCvtAppTypeM #-}
debugCvtAppTypeM :: (?ienv :: ImplicitEnv) => GHC.Id -> [GHC.StgArg] -> GHC.Type -> String -> IO Type
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

cvtType :: (?ienv :: ImplicitEnv) => String -> GHC.Type -> Type
--cvtType msg t = trpp (unwords [msg, "cvtType"]) cvtType3 $ deepCheckType t
--cvtType msg t = trpp (unwords [msg, "cvtType"]) cvtTypeNormal t
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
cvtTypeNormal :: (?ienv :: ImplicitEnv) => GHC.Type -> Type
cvtTypeNormal t
  | not (GHC.typeHasFixedRuntimeRep t)
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
  GHC.LitNumBigNat  -> error $ "cvtLitNumType: LitNumBigNat is not supported"

cvtLabelSpec :: Maybe Int -> GHC.FunctionOrData -> LabelSpec
cvtLabelSpec mi = \case
  GHC.IsFunction  -> FunctionLabel mi
  GHC.IsData      -> DataLabel

cvtLit :: (?ienv :: ImplicitEnv) => GHC.Literal -> Lit
cvtLit = \case
  GHC.LitChar x       -> LitChar x
  GHC.LitString x     -> LitString x
  GHC.LitNullAddr     -> LitNullAddr
  GHC.LitFloat x      -> LitFloat x
  GHC.LitDouble x     -> LitDouble x
  GHC.LitLabel x i d  -> LitLabel (GHC.bytesFS  x) (cvtLabelSpec i d)
  GHC.LitNumber t i   -> LitNumber (cvtLitNumType t) i
  r@GHC.LitRubbish{}  -> LitRubbish (cvtType "LitRubbish" $ GHC.literalType r)

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

cvtCbvMark :: GHC.CbvMark -> CbvMark
cvtCbvMark = \case
  GHC.MarkedCbv     -> MarkedCbv
  GHC.NotMarkedCbv  -> NotMarkedCbv

cvtIdDetails :: GHC.Id -> M IdDetails
cvtIdDetails i = case GHC.idDetails i of
  GHC.VanillaId       -> pure VanillaId
  GHC.RecSelId{}      -> pure RecSelId
  GHC.DataConWorkId d -> DataConWorkId <$> cvtDataCon d
  GHC.DataConWrapId d -> DataConWrapId <$> cvtDataCon d
  GHC.ClassOpId{}     -> pure ClassOpId
  GHC.PrimOpId{}      -> pure PrimOpId
  GHC.FCallId{}       -> pure FCallId
  GHC.TickBoxOpId{}   -> pure TickBoxOpId
  GHC.DFunId{}        -> pure DFunId
  GHC.CoVarId{}       -> pure CoVarId
  GHC.JoinId ar m     -> pure $ JoinId ar (fmap (map cvtCbvMark) m)
  GHC.WorkerLikeId l  -> pure $ WorkerLikeId $ map cvtCbvMark l

cvtScope :: (?ienv :: ImplicitEnv) => GHC.Id -> Scope
cvtScope i
  | GHC.isExportedId i  = ModulePublic
  | otherwise           = ClosurePrivate

cvtBinderIdClosureParam :: (?ienv :: ImplicitEnv) => IdDetails -> String -> GHC.Id -> SBinder
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


cvtBinderId :: (?ienv :: ImplicitEnv) => IdDetails -> String -> GHC.Id -> SBinder
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

cvtBinderIdClosureParamM :: (?ienv :: ImplicitEnv) => String -> GHC.Id -> M SBinder
cvtBinderIdClosureParamM msg i = do
  --defKey i -- debug
  details <- cvtIdDetails i
  pure $ cvtBinderIdClosureParam details msg i

cvtBinderIdM :: (?ienv :: ImplicitEnv) => String -> GHC.Id -> M SBinder
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

cvtArg :: (?ienv :: ImplicitEnv) => GHC.StgArg -> M SArg
cvtArg = \case
  GHC.StgVarArg o -> StgVarArg <$> cvtOccId o
  GHC.StgLitArg l -> pure $ StgLitArg (cvtLit l)

-- alt conversion

cvtAltType :: (?ienv :: ImplicitEnv) => GHC.AltType -> M SAltType
cvtAltType = \case
  GHC.PolyAlt       -> pure $ PolyAlt
  GHC.MultiValAlt i -> pure $ MultiValAlt i
  GHC.PrimAlt r     -> pure $ PrimAlt $ cvtPrimRep r
  GHC.AlgAlt tc     -> AlgAlt <$> cvtDataTyConId tc

cvtAlt :: (?ienv :: ImplicitEnv) => GHC.CgStgAlt -> M SAlt
cvtAlt (GHC.GenStgAlt con bs e) = Alt <$> cvtAltCon con <*> mapM (cvtBinderIdM "Alt") bs <*> cvtExpr e

cvtAltCon :: (?ienv :: ImplicitEnv) => GHC.AltCon -> M SAltCon
cvtAltCon = \case
  GHC.DataAlt con -> AltDataCon <$> cvtDataCon con
  GHC.LitAlt l    -> pure . AltLit $ cvtLit l
  GHC.DEFAULT     -> pure $ AltDefault

-- stg expr conversion

-- WORKAROUND for rewriteRhs in compiler/GHC/Stg/InferTags/Rewrite.hs
cvtConAppTypeArgs :: (?ienv :: ImplicitEnv) => [GHC.Type] -> M [Type]
cvtConAppTypeArgs tys = pure . unsafePerformIO $ catch (evaluate $ map (cvtType "cvtConAppTypeArgs") tys) $ \case
  GHC.Panic msg
    | "mkSeqs shouldn't use the type arg" `isInfixOf` msg
    -> pure []
  e -> throw e

cvtExpr :: (?ienv :: ImplicitEnv) => GHC.CgStgExpr -> M SExpr
cvtExpr = \case
  GHC.StgApp f ps           -> StgApp <$> cvtOccId f <*> mapM cvtArg ps
  GHC.StgLit l              -> pure $ StgLit (cvtLit l)
  GHC.StgConApp dc _ ps ts  -> StgConApp <$> cvtDataCon dc <*> mapM cvtArg ps <*> cvtConAppTypeArgs ts
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

cvtRhs :: (?ienv :: ImplicitEnv) => GHC.CgStgRhs -> M SRhs
cvtRhs = \case
  GHC.StgRhsClosure _ _ u bs e  -> StgRhsClosure [] (cvtUpdateFlag u) <$> mapM (cvtBinderIdClosureParamM "StgRhsClosure") bs <*> cvtExpr e
  GHC.StgRhsCon _ dc _ _ args   -> StgRhsCon <$> cvtDataCon dc <*> mapM cvtArg args

-- bind and top-bind conversion

cvtBind :: (?ienv :: ImplicitEnv) => GHC.CgStgBinding -> M SBinding
cvtBind = \case
  GHC.StgNonRec b r -> StgNonRec <$> cvtBinderIdM "StgNonRec" b <*> cvtRhs r
  GHC.StgRec    bs  -> StgRec <$> sequence [(,) <$> cvtBinderIdM "StgRec" b <*> cvtRhs r | (b, r) <- bs]

cvtTopBind :: (?ienv :: ImplicitEnv) => GHC.CgStgTopBinding -> M STopBinding
cvtTopBind = \case
  GHC.StgTopLifted b        -> StgTopLifted <$> cvtBind b
  GHC.StgTopStringLit b bs  -> StgTopStringLit <$> cvtBinderIdM "StgTopStringLit" b <*> pure bs

cvtTopBindsAndStubs :: (?ienv :: ImplicitEnv) => [GHC.CgStgTopBinding] -> GHC.ForeignStubs -> WPC.ForeignStubDecls -> M ([STopBinding], SForeignStubs, [(UnitId, [(ModuleName, [SBinder])])])
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

cvtStubImpl :: (?ienv :: ImplicitEnv) => WPC.StubImpl -> StubImpl
cvtStubImpl = \case
  WPC.StubImplImportCWrapper n m b r a -> StubImplImportCWrapper (GHC.bytesFS n) m b (BS8.pack r) (map BS8.pack a)

cvtForeignImport :: GHC.ForeignImport GHC.GhcTc -> ForeignImport
cvtForeignImport (GHC.CImport t cc s m is) = CImport (cvtCCallConv $ GHC.unLoc cc) (cvtSafety $ GHC.unLoc s) (fmap cvtHeader m) (cvtCImportSpec is) (cvtSourceText $ GHC.unLoc t)

cvtForeignExport :: GHC.ForeignExport GHC.GhcTc -> ForeignExport
cvtForeignExport (GHC.CExport t s) = CExport (cvtCExportSpec $ GHC.unLoc s) (cvtSourceText $ GHC.unLoc t)

cvtStubDecl :: (?ienv :: ImplicitEnv) => WPC.StubDecl -> M SStubDecl
cvtStubDecl = \case
  WPC.StubDeclImport fi m     -> pure $ StubDeclImport (cvtForeignImport fi) (fmap cvtStubImpl m)
  WPC.StubDeclExport fe idOcc -> StubDeclExport (cvtForeignExport fe) <$> cvtOccId idOcc <*> pure (BS8.pack "") -- TODO: remove this

cvtForeignStubs :: (?ienv :: ImplicitEnv) => GHC.ForeignStubs -> WPC.ForeignStubDecls -> M SForeignStubs
cvtForeignStubs stubs (WPC.ForeignStubDecls decls) = case stubs of
  GHC.NoStubs
    -> pure NoStubs
  GHC.ForeignStubs (GHC.CHeader h) (GHC.CStub c iList fList)
    -> ForeignStubs
        (bs8SDoc $ GHC.pprCode h)
        (bs8SDoc $ GHC.pprCode c)
        [] -- TODO: (map cvtModuleCLabel iList)
        [] -- TODO: (map cvtModuleCLabel fList)
        <$> mapM cvtStubDecl [d | (_, d) <- decls]
{-
cvtModuleLabelKind :: GHC.ModuleLabelKind -> ModuleLabelKind
cvtModuleLabelKind = \case
  GHC.MLK_Initializer (GHC.LexicalFastString n) -> MLK_Initializer  (GHC.bytesFS n)
  GHC.MLK_Finalizer   (GHC.LexicalFastString n) -> MLK_Finalizer    (GHC.bytesFS n)
  GHC.MLK_InitializerArray  -> MLK_InitializerArray
  GHC.MLK_FinalizerArray    -> MLK_FinalizerArray
  GHC.MLK_IPEBuffer         -> MLK_IPEBuffer

cvtModuleCLabel :: (?ienv :: ImplicitEnv) => GHC.CLabel -> ModuleCLabel
cvtModuleCLabel clbl = case GHC.deconstructModuleLabel_maybe clbl of
  Nothing     -> error $ "unsupported CLabel: " ++ show clbl
  Just (m, k) -> ModuleCLabel (cvtUnitId $ GHC.moduleUnit m) (cvtModuleName $ GHC.moduleName m) (cvtModuleLabelKind k)
-}

-- module conversion

cvtModule :: GHC.DynFlags -> String -> GHC.Unit -> GHC.ModuleName -> Maybe FilePath -> [GHC.CgStgTopBinding] -> GHC.ForeignStubs -> WPC.ForeignStubDecls -> SModule
cvtModule dflags phase unit' modName' mSrcPath binds foreignStubs foreignDecls =
  let ?ienv = ImplicitEnv dflags
  in cvtModule' phase unit' modName' mSrcPath binds foreignStubs foreignDecls

cvtModule' :: (?ienv :: ImplicitEnv) => String -> GHC.Unit -> GHC.ModuleName -> Maybe FilePath -> [GHC.CgStgTopBinding] -> GHC.ForeignStubs -> WPC.ForeignStubDecls -> SModule
cvtModule' phase unit' modName' mSrcPath binds foreignStubs foreignDecls@(WPC.ForeignStubDecls fDecls) =
  Module
  { modulePhase               = BS8.pack phase
  , moduleUnitId              = unitId
  , moduleName                = modName
  , moduleSourceFilePath      = fmap BS8.pack mSrcPath
  , moduleForeignStubs        = stubs
  , moduleHasForeignExported  = not $ null [id | (_, WPC.StubDeclExport _ id) <- fDecls]
  , moduleDependency          = dependencies
  , moduleExternalTopIds      = externalIds
  , moduleTyCons              = tyCons
  , moduleTopBindings         = topBinds
  } where

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

mkExternalName :: (?ienv :: ImplicitEnv) => GHC.Id -> M ((UnitId, ModuleName), SBinder)
mkExternalName x = (cvtUnitIdAndModuleName . GHC.nameModule $ GHC.getName x,) <$> cvtBinderIdM "mkExternalName" x

mkTyCon :: (?ienv :: ImplicitEnv) => GHC.TyCon -> ((UnitId, ModuleName), STyCon)
mkTyCon tc = (cvtUnitIdAndModuleName $ GHC.nameModule n, b) where
  n = GHC.getName tc
  b = STyCon
      { stcName     = cvtOccName $ GHC.getOccName n
      , stcId       = TyConId . cvtUnique . GHC.getUnique $ n
      , stcDataCons = map mkSDataCon . sortDataCons $ GHC.tyConDataCons tc
      , stcDefLoc   = cvtSrcSpan $ GHC.nameSrcSpan n
      }
  sortDataCons l = IntMap.elems $ IntMap.fromList [(GHC.dataConTag dc, dc) | dc <- l]

mkSDataCon :: (?ienv :: ImplicitEnv) => GHC.DataCon -> SDataCon
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

topBindIds :: GHC.CgStgTopBinding -> [GHC.Id]
topBindIds = \case
  GHC.StgTopLifted (GHC.StgNonRec b _)  -> [b]
  GHC.StgTopLifted (GHC.StgRec bs)      ->  map fst bs
  GHC.StgTopStringLit b _               -> [b]
