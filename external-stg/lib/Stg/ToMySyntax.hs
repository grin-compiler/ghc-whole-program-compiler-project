{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.ToMySyntax (cvtSModule) where

import Data.Ratio
import qualified Data.ByteString.Char8 as BS8

import Stg.Syntax
import qualified Stg.MySyntax as My

cvtIdInfo :: IdInfo -> My.IdInfo
cvtIdInfo = BS8.unpack

cvtName :: Name -> My.StgName
cvtName = BS8.unpack

cvtUnique :: Unique -> My.Unique
cvtUnique = \case
  Unique a b -> My.MkUnique a b

cvtRealSrcSpan :: RealSrcSpan -> My.RealSrcSpan
cvtRealSrcSpan = \case
  RealSrcSpan' a b c d e -> My.MkRealSrcSpan' (cvtName a) b c d e

cvtBufSpan :: BufSpan -> My.BufSpan
cvtBufSpan = \case
  BufSpan a b -> My.MkBufSpan a b

cvtUnhelpfulSpanReason :: UnhelpfulSpanReason -> My.UnhelpfulSpanReason
cvtUnhelpfulSpanReason = \case
  UnhelpfulNoLocationInfo -> My.UnhelpfulNoLocationInfo
  UnhelpfulWiredIn        -> My.UnhelpfulWiredIn
  UnhelpfulInteractive    -> My.UnhelpfulInteractive
  UnhelpfulGenerated      -> My.UnhelpfulGenerated
  UnhelpfulOther a        -> My.UnhelpfulOther $ cvtName a

cvtSrcSpan :: SrcSpan -> My.SrcSpan
cvtSrcSpan = \case
  RealSrcSpan a b -> My.MkRealSrcSpan (cvtRealSrcSpan a) (fmap cvtBufSpan b)
  UnhelpfulSpan a -> My.UnhelpfulSpan $ cvtUnhelpfulSpanReason a

cvtTickish :: Tickish -> My.Tickish
cvtTickish = \case
  ProfNote    -> My.ProfNote
  HpcTick     -> My.HpcTick
  Breakpoint  -> My.Breakpoint
  SourceNote a b -> My.SourceNote (cvtRealSrcSpan a) (cvtName b)

cvtPrimElemRep :: PrimElemRep -> My.PrimElemRep
cvtPrimElemRep = \case
  Int8ElemRep   -> My.Int8ElemRep
  Int16ElemRep  -> My.Int16ElemRep
  Int32ElemRep  -> My.Int32ElemRep
  Int64ElemRep  -> My.Int64ElemRep
  Word8ElemRep  -> My.Word8ElemRep
  Word16ElemRep -> My.Word16ElemRep
  Word32ElemRep -> My.Word32ElemRep
  Word64ElemRep -> My.Word64ElemRep
  FloatElemRep  -> My.FloatElemRep
  DoubleElemRep -> My.DoubleElemRep

cvtPrimRep :: PrimRep -> My.PrimRep
cvtPrimRep = \case
  VoidRep     -> My.VoidRep
  LiftedRep   -> My.LiftedRep
  UnliftedRep -> My.UnliftedRep
  Int8Rep     -> My.Int8Rep
  Int16Rep    -> My.Int16Rep
  Int32Rep    -> My.Int32Rep
  Int64Rep    -> My.Int64Rep
  IntRep      -> My.IntRep
  Word8Rep    -> My.Word8Rep
  Word16Rep   -> My.Word16Rep
  Word32Rep   -> My.Word32Rep
  Word64Rep   -> My.Word64Rep
  WordRep     -> My.WordRep
  AddrRep     -> My.AddrRep
  FloatRep    -> My.FloatRep
  DoubleRep   -> My.DoubleRep
  VecRep a b  -> My.VecRep a (cvtPrimElemRep b)

cvtType :: Type -> My.StgType
cvtType = \case
  SingleValue a   -> My.SingleValue $ cvtPrimRep a
  UnboxedTuple b  -> My.UnboxedTuple $ map cvtPrimRep b
  PolymorphicRep  -> My.PolymorphicRep

cvtTyConId :: TyConId -> My.TyConId
cvtTyConId = \case
  TyConId a -> My.MkTyConId $ cvtUnique a

cvtDataConId :: DataConId -> My.DataConId
cvtDataConId = \case
  DataConId a -> My.MkDataConId $ cvtUnique a

cvtDataConRep :: DataConRep -> My.DataConRep
cvtDataConRep = \case
  AlgDataCon a      -> My.AlgDataCon $ map cvtPrimRep a
  UnboxedTupleCon a -> My.UnboxedTupleCon a

cvtUnitId :: UnitId -> My.UnitId
cvtUnitId = \case
  UnitId a -> My.MkUnitId $ cvtName a

cvtModuleName :: ModuleName -> My.ModuleName
cvtModuleName = \case
  ModuleName a -> My.MkModuleName $ cvtName a

cvtCbvMark :: CbvMark -> My.CbvMark
cvtCbvMark = \case
  MarkedCbv     -> My.MarkedCbv
  NotMarkedCbv  -> My.NotMarkedCbv

cvtIdDetails :: IdDetails -> My.IdDetails
cvtIdDetails = \case
  VanillaId       -> My.VanillaId
  RecSelId        -> My.RecSelId
  DataConWorkId a -> My.DataConWorkId $ cvtDataConId a
  DataConWrapId a -> My.DataConWrapId $ cvtDataConId a
  ClassOpId       -> My.ClassOpId
  PrimOpId        -> My.PrimOpId
  FCallId         -> My.FCallId
  TickBoxOpId     -> My.TickBoxOpId
  DFunId          -> My.DFunId
  CoVarId         -> My.CoVarId
  JoinId a b      -> My.JoinId a $ fmap (map cvtCbvMark) b
  WorkerLikeId a  -> My.WorkerLikeId $ map cvtCbvMark a
  RepPolyId       -> My.RepPolyId

cvtBinderId :: BinderId -> My.BinderId
cvtBinderId = \case
  BinderId a  -> My.MkBinderId $ cvtUnique a

cvtScope :: Scope -> My.Scope
cvtScope = \case
  ModulePublic    -> My.ModulePublic
  ModulePrivate   -> My.ModulePrivate
  ClosurePrivate  -> My.ClosurePrivate

cvtLitNumType :: LitNumType -> My.LitNumType
cvtLitNumType = \case
  LitNumInt     -> My.LitNumInt
  LitNumInt8    -> My.LitNumInt8
  LitNumInt16   -> My.LitNumInt16
  LitNumInt32   -> My.LitNumInt32
  LitNumInt64   -> My.LitNumInt64
  LitNumWord    -> My.LitNumWord
  LitNumWord8   -> My.LitNumWord8
  LitNumWord16  -> My.LitNumWord16
  LitNumWord32  -> My.LitNumWord32
  LitNumWord64  -> My.LitNumWord64

cvtLabelSpec :: LabelSpec -> My.LabelSpec
cvtLabelSpec = \case
  FunctionLabel a -> My.FunctionLabel a
  DataLabel       -> My.DataLabel

cvtLit :: Lit -> My.Lit
cvtLit = \case
  LitChar a     -> My.LitChar a
  LitString a   -> My.LitString $ BS8.unpack a
  LitNullAddr   -> My.LitNullAddr
  LitFloat a    -> My.LitFloat (numerator a) (denominator a)
  LitDouble a   -> My.LitDouble (numerator a) (denominator a)
  LitLabel a b  -> My.LitLabel (BS8.unpack a) (cvtLabelSpec b)
  LitNumber a b -> My.LitNumber (cvtLitNumType a) b
  LitRubbish a  -> My.LitRubbish $ cvtType a

cvtSAltType :: SAltType -> MySAltType
cvtSAltType = \case
  PolyAlt       -> My.PolyAlt
  MultiValAlt a -> My.MultiValAlt a
  PrimAlt a     -> My.PrimAlt $ cvtPrimRep a
  AlgAlt a      -> My.AlgAlt $ cvtTyConId a

cvtUpdateFlag :: UpdateFlag -> My.UpdateFlag
cvtUpdateFlag = \case
  ReEntrant   -> My.ReEntrant
  Updatable   -> My.Updatable
  SingleEntry -> My.SingleEntry

cvtSArg :: SArg -> MySArg
cvtSArg = \case
  StgVarArg a -> My.StgVarArg $ cvtBinderId a
  StgLitArg a -> My.StgLitArg $ cvtLit a

cvtSAltCon :: SAltCon -> MySAltCon
cvtSAltCon = \case
  AltDataCon a  -> My.AltDataCon $ cvtDataConId a
  AltLit a      -> My.AltLit $ cvtLit a
  AltDefault    -> My.AltDefault

cvtSafety :: Safety -> My.Safety
cvtSafety = \case
  PlaySafe          -> My.PlaySafe
  PlayInterruptible -> My.PlayInterruptible
  PlayRisky         -> My.PlayRisky

cvtCCallConv :: CCallConv -> My.CCallConv
cvtCCallConv = \case
  CCallConv           -> My.MkCCallConv
  CApiConv            -> My.CApiConv
  StdCallConv         -> My.StdCallConv
  PrimCallConv        -> My.PrimCallConv
  JavaScriptCallConv  -> My.JavaScriptCallConv

cvtSourceText :: SourceText -> My.SourceText
cvtSourceText = \case
  SourceText a  -> My.MkSourceText $ BS8.unpack a
  NoSourceText  -> My.NoSourceText

cvtCCallTarget :: CCallTarget -> My.CCallTarget
cvtCCallTarget = \case
  StaticTarget a b c d  -> My.StaticTarget (cvtSourceText a) (BS8.unpack b) (fmap cvtUnitId c) d
  DynamicTarget         -> My.DynamicTarget

cvtPrimCall :: PrimCall -> My.PrimCall
cvtPrimCall = \case
  PrimCall a b -> My.MkPrimCall (BS8.unpack a) (cvtUnitId b)

cvtForeignCall :: ForeignCall -> My.ForeignCall
cvtForeignCall = \case
  ForeignCall a b c -> My.MkForeignCall (cvtCCallTarget a) (cvtCCallConv b) (cvtSafety c)

cvtStgOp :: StgOp -> My.StgOp
cvtStgOp = \case
  StgPrimOp a     -> My.StgPrimOp $ cvtName a
  StgPrimCallOp a -> My.StgPrimCallOp $ cvtPrimCall a
  StgFCallOp a    -> My.StgFCallOp $ cvtForeignCall a

cvtSBinding :: SBinding -> MySBinding
cvtSBinding = \case
  StgNonRec a b -> My.StgNonRec (cvtSBinder a) (cvtSRhs b)
  StgRec l -> My.StgRec [(cvtSBinder a, cvtSRhs b) | (a, b) <- l]

cvtSTopBinding :: STopBinding -> MySTopBinding
cvtSTopBinding = \case
  StgTopLifted a      -> My.StgTopLifted $ cvtSBinding a
  StgTopStringLit a b -> My.StgTopStringLit (cvtSBinder a) (BS8.unpack b)

cvtSExpr :: SExpr -> MySExpr
cvtSExpr = \case
  StgApp a b        -> My.StgApp (cvtBinderId a) (map cvtSArg b)
  StgLit a          -> My.StgLit $ cvtLit a
  StgConApp a b c   -> My.StgConApp (cvtDataConId a) (map cvtSArg b) (map cvtType c)
  StgOpApp a b c d  -> My.StgOpApp (cvtStgOp a) (map cvtSArg b) (cvtType c) (fmap cvtTyConId d)
  StgCase a b c d   -> My.StgCase (cvtSExpr a) (cvtSBinder b) (cvtSAltType c) (map cvtSAlt d)
  StgLet a b        -> My.StgLet (cvtSBinding a) (cvtSExpr b)
  StgLetNoEscape a b -> My.StgLetNoEscape (cvtSBinding a) (cvtSExpr b)
  StgTick a b       -> My.StgTick (cvtTickish a) (cvtSExpr b)

cvtSRhs :: SRhs -> MySRhs
cvtSRhs = \case
  StgRhsClosure a b c d -> My.StgRhsClosure (map cvtBinderId a) (cvtUpdateFlag b) (map cvtSBinder c) (cvtSExpr d)
  StgRhsCon a b         -> My.StgRhsCon (cvtDataConId a) (map cvtSArg b)

cvtSAlt :: SAlt -> MySAlt
cvtSAlt = \case
  Alt a b c -> My.MkAlt (cvtSAltCon a) (map cvtSBinder b) (cvtSExpr c)

cvtHeader :: Header -> My.Header
cvtHeader = \case
  Header a b -> My.MkHeader (cvtSourceText a) (cvtName b)

cvtCImportSpec :: CImportSpec -> My.CImportSpec
cvtCImportSpec = \case
  CLabel a    -> My.CLabel $ cvtName a
  CFunction b -> My.CFunction $ cvtCCallTarget b
  CWrapper    -> My.CWrapper

cvtCExportSpec :: CExportSpec -> My.CExportSpec
cvtCExportSpec = \case
  CExportStatic a b c -> My.CExportStatic (cvtSourceText a) (cvtName b) (cvtCCallConv c)

cvtForeignImport :: ForeignImport -> My.ForeignImport
cvtForeignImport = \case
  CImport a b c d e -> My.CImport (cvtCCallConv a) (cvtSafety b) (fmap cvtHeader c) (cvtCImportSpec d) (cvtSourceText e)

cvtForeignExport :: ForeignExport -> My.ForeignExport
cvtForeignExport = \case
  CExport a b -> My.CExport (cvtCExportSpec a) (cvtSourceText b)

cvtStubImpl :: StubImpl -> My.StubImpl
cvtStubImpl = \case
  StubImplImportCWrapper a b c d e  -> My.StubImplImportCWrapper (cvtName a) b c (cvtName d) (map cvtName e)
  StubImplImportCApi a l            -> My.StubImplImportCApi (cvtName a) [(fmap cvtHeader x, (BS8.unpack y, z)) | (x, y, z) <- l]

cvtSStubDecl :: SStubDecl -> MySStubDecl
cvtSStubDecl = \case
  StubDeclImport a b    -> My.StubDeclImport (cvtForeignImport a) (fmap cvtStubImpl b)
  StubDeclExport a b c  -> My.StubDeclExport (cvtForeignExport a) (cvtBinderId b) (BS8.unpack c)

cvtModuleLabelKind :: ModuleLabelKind -> My.ModuleLabelKind
cvtModuleLabelKind = \case
  MLK_Initializer a     -> My.MLK_Initializer $ cvtName a
  MLK_InitializerArray  -> My.MLK_InitializerArray
  MLK_Finalizer a       -> My.MLK_Finalizer $ cvtName a
  MLK_FinalizerArray    -> My.MLK_FinalizerArray
  MLK_IPEBuffer         -> My.MLK_IPEBuffer

cvtModuleCLabel :: ModuleCLabel -> My.ModuleCLabel
cvtModuleCLabel = \case
  ModuleCLabel a b c -> My.MkModuleCLabel (cvtUnitId a) (cvtModuleName b) (cvtModuleLabelKind c)

cvtSDataCon :: SDataCon -> My.SDataCon
cvtSDataCon = \case
  SDataCon a b c d e -> My.MkSDataCon (cvtName a) (cvtDataConId b) (cvtDataConRep c) (cvtSBinder d) (cvtSrcSpan e)

cvtSTyCon :: STyCon -> My.STyCon
cvtSTyCon = \case
  STyCon a b c d -> My.MkSTyCon (cvtName a) (cvtTyConId b) (map cvtSDataCon c) (cvtSrcSpan d)

cvtSBinder :: SBinder -> My.SBinder
cvtSBinder = \case
  SBinder a b c d e f g h -> My.MkSBinder (cvtName a) (cvtBinderId b) (cvtType c) (cvtName d) (cvtScope e) (cvtIdDetails f) (cvtIdInfo g) (cvtSrcSpan h)

cvtSForeignStubs :: SForeignStubs -> MySForeignStubs
cvtSForeignStubs = \case
  NoStubs                 -> My.NoStubs
  ForeignStubs a b c d e  -> My.MkForeignStubs (BS8.unpack a) (BS8.unpack b) (map cvtModuleCLabel c) (map cvtModuleCLabel d) (map cvtSStubDecl e)

cvtSModule :: SModule -> MySModule
cvtSModule = \case
  Module a b c d e f g h i j -> My.MkModule (BS8.unpack a) (cvtUnitId b) (cvtModuleName c) (fmap cvtName d) (cvtSForeignStubs e) f
                                  [(cvtUnitId x, map cvtModuleName y) | (x, y) <- g]
                                  [(cvtUnitId x, [(cvtModuleName p, map cvtSBinder q) | (p, q) <- y]) | (x, y) <- h]
                                  [(cvtUnitId x, [(cvtModuleName p, map cvtSTyCon q)  | (p, q) <- y]) | (x, y) <- i]
                                  (map cvtSTopBinding j)

type MySModule        = My.Module'       My.SBinder My.BinderId My.DataConId My.STyCon  My.TyConId
type MySTopBinding    = My.TopBinding'   My.SBinder My.BinderId My.DataConId My.TyConId
type MySBinding       = My.Binding'      My.SBinder My.BinderId My.DataConId My.TyConId
type MySExpr          = My.Expr'         My.SBinder My.BinderId My.DataConId My.TyConId
type MySRhs           = My.Rhs'          My.SBinder My.BinderId My.DataConId My.TyConId
type MySAlt           = My.Alt'          My.SBinder My.BinderId My.DataConId My.TyConId
type MySAltCon        = My.AltCon'       My.DataConId
type MySAltType       = My.AltType'      My.TyConId
type MySArg           = My.Arg'          My.BinderId
type MySStubDecl      = My.StubDecl'     My.BinderId
type MySForeignStubs  = My.ForeignStubs' My.BinderId
