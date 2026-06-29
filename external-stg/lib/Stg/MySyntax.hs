{-# LANGUAGE DeriveGeneric #-}
module Stg.MySyntax where

import GHC.Generics
import Data.Aeson

jsonTaggedObject :: SumEncoding
jsonTaggedObject = TaggedObject
              { tagFieldName      = "tag"
              , contentsFieldName = "contents"
              }

jsonOptions :: Options
jsonOptions = defaultOptions
         { fieldLabelModifier      = id
         , constructorTagModifier  = id
         , allNullaryToStringTag   = False
         , omitNothingFields       = False
         -- , allowOmittedFields      = True
         , sumEncoding             = TwoElemArray -- jsonTaggedObject
         , unwrapUnaryRecords      = False
         , tagSingleConstructors   = True
         , rejectUnknownFields     = False
         }

type List a = [a]
type Pair a b = (a, b)

type StgName = String
type IdInfo = String
data Unique
  = MkUnique Char Int
  deriving Generic

data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulWiredIn
  | UnhelpfulInteractive
  | UnhelpfulGenerated
  | UnhelpfulOther StgName
  deriving Generic

data SrcSpan
  = MkRealSrcSpan RealSrcSpan (Maybe BufSpan)
  | UnhelpfulSpan UnhelpfulSpanReason
  deriving Generic

data Tickish
  = ProfNote
  | HpcTick
  | Breakpoint
  | SourceNote RealSrcSpan StgName
  deriving Generic

data PrimElemRep
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep
  deriving Generic

data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep
  | Int8Rep
  | Int16Rep
  | Int32Rep
  | Int64Rep
  | IntRep
  | Word8Rep
  | Word16Rep
  | Word32Rep
  | Word64Rep
  | WordRep
  | AddrRep
  | FloatRep
  | DoubleRep
  | VecRep Int PrimElemRep
  deriving Generic

data StgType
  = SingleValue PrimRep
  | UnboxedTuple (List PrimRep)
  | PolymorphicRep
  deriving Generic

data TyConId
  = MkTyConId Unique
  deriving Generic

data DataConId
  = MkDataConId Unique
  deriving Generic

data DataConRep
  = AlgDataCon (List PrimRep)
  | UnboxedTupleCon Int
  deriving Generic

data UnitId
  = MkUnitId StgName
  deriving Generic

data ModuleName
  = MkModuleName StgName
  deriving Generic

data CbvMark
  = MarkedCbv
  | NotMarkedCbv
  deriving Generic

data IdDetails
  = VanillaId
  | RecSelId
  | DataConWorkId DataConId
  | DataConWrapId DataConId
  | ClassOpId
  | PrimOpId
  | FCallId
  | TickBoxOpId
  | DFunId
  | CoVarId
  | JoinId Int (Maybe (List CbvMark))
  | WorkerLikeId (List CbvMark)
  | RepPolyId
  deriving Generic

data BinderId
  = MkBinderId Unique
  deriving Generic

data Scope
  = ModulePublic
  | ModulePrivate
  | ClosurePrivate
  deriving Generic

data LitNumType
  = LitNumInt
  | LitNumInt8
  | LitNumInt16
  | LitNumInt32
  | LitNumInt64
  | LitNumWord
  | LitNumWord8
  | LitNumWord16
  | LitNumWord32
  | LitNumWord64
  deriving Generic

data LabelSpec
  = FunctionLabel (Maybe Int)
  | DataLabel
  deriving Generic

data Lit
  = LitChar Char
  | LitString String
  | LitNullAddr
  | LitFloat Integer Integer
  | LitDouble Integer Integer
  | LitLabel String LabelSpec
  | LitNumber LitNumType Integer
  | LitRubbish StgType
  deriving Generic

data AltType' tcOcc
  = PolyAlt
  | MultiValAlt Int
  | PrimAlt PrimRep
  | AlgAlt tcOcc
  deriving Generic

data UpdateFlag
  = ReEntrant
  | Updatable
  | SingleEntry
  deriving Generic

data Arg' idOcc
  = StgVarArg idOcc
  | StgLitArg Lit
  deriving Generic

data AltCon' dcOcc
  = AltDataCon dcOcc
  | AltLit Lit
  | AltDefault
  deriving Generic

data Safety
  = PlaySafe
  | PlayInterruptible
  | PlayRisky
  deriving Generic

data CCallConv
  = MkCCallConv
  | CApiConv
  | StdCallConv
  | PrimCallConv
  | JavaScriptCallConv
  deriving Generic

data SourceText
  = MkSourceText String
  | NoSourceText
  deriving Generic

data CCallTarget
  = StaticTarget SourceText String (Maybe UnitId) Bool
  | DynamicTarget
  deriving Generic

data PrimCall
  = MkPrimCall String UnitId
  deriving Generic

data StgOp
  = StgPrimOp StgName
  | StgPrimCallOp PrimCall
  | StgFCallOp ForeignCall
  deriving Generic

data Binding' idBnd idOcc dcOcc tcOcc
  = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
  | StgRec (List (Pair idBnd (Rhs' idBnd idOcc dcOcc tcOcc)))
  deriving Generic

data TopBinding' idBnd idOcc dcOcc tcOcc
  = StgTopLifted (Binding' idBnd idOcc dcOcc tcOcc)
  | StgTopStringLit idBnd String
  deriving Generic

data Expr' idBnd idOcc dcOcc tcOcc
  = StgApp idOcc (List (Arg' idOcc))
  | StgLit Lit
  | StgConApp dcOcc (List (Arg' idOcc)) (List StgType)
  | StgOpApp StgOp (List (Arg' idOcc)) StgType (Maybe tcOcc)
  | StgCase (Expr' idBnd idOcc dcOcc tcOcc) idBnd (AltType' tcOcc) (List (Alt' idBnd idOcc dcOcc tcOcc))
  | StgLet (Binding' idBnd idOcc dcOcc tcOcc) (Expr' idBnd idOcc dcOcc tcOcc)
  | StgLetNoEscape (Binding' idBnd idOcc dcOcc tcOcc) (Expr' idBnd idOcc dcOcc tcOcc)
  | StgTick Tickish (Expr' idBnd idOcc dcOcc tcOcc)
  deriving Generic

data Rhs' idBnd idOcc dcOcc tcOcc
  = StgRhsClosure (List idOcc) UpdateFlag (List idBnd) (Expr' idBnd idOcc dcOcc tcOcc)
  | StgRhsCon dcOcc (List (Arg' idOcc))
  deriving Generic

data Alt' idBnd idOcc dcOcc tcOcc
  = MkAlt (AltCon' dcOcc) (List idBnd) (Expr' idBnd idOcc dcOcc tcOcc)
  deriving Generic

data Header
  = MkHeader SourceText StgName
  deriving Generic

data CImportSpec
  = CLabel StgName
  | CFunction CCallTarget
  | CWrapper
  deriving Generic

data CExportSpec
  = CExportStatic SourceText StgName CCallConv
  deriving Generic

data ForeignImport
  = CImport CCallConv Safety (Maybe Header) CImportSpec SourceText
  deriving Generic

data ForeignExport
  = CExport CExportSpec SourceText
  deriving Generic

data StubImpl
  = StubImplImportCWrapper StgName (Maybe Int) Bool StgName (List StgName)
  | StubImplImportCApi StgName (List (Pair (Maybe Header) (Pair String Char)))
  deriving Generic

data StubDecl' idOcc
  = StubDeclImport ForeignImport (Maybe StubImpl)
  | StubDeclExport ForeignExport idOcc String
  deriving Generic

data ModuleLabelKind
  = MLK_Initializer StgName
  | MLK_InitializerArray
  | MLK_Finalizer StgName
  | MLK_FinalizerArray
  | MLK_IPEBuffer
  deriving Generic

data ModuleCLabel
  = MkModuleCLabel UnitId ModuleName ModuleLabelKind
  deriving Generic

data RealSrcSpan
  = MkRealSrcSpan'
      StgName
      Int
      Int
      Int
      Int
  deriving Generic

data BufSpan
  = MkBufSpan
      Int
      Int
  deriving Generic

data SDataCon
  = MkSDataCon
      StgName
      DataConId
      DataConRep
      SBinder
      SrcSpan
  deriving Generic

data STyCon
  = MkSTyCon
      StgName
      TyConId
      (List SDataCon)
      SrcSpan
  deriving Generic

data SBinder
  = MkSBinder
      StgName
      BinderId
      StgType
      StgName
      Scope
      IdDetails
      IdInfo
      SrcSpan
  deriving Generic

data ForeignCall
  = MkForeignCall
      CCallTarget
      CCallConv
      Safety
  deriving Generic

data ForeignStubs' idOcc
  = NoStubs
  | MkForeignStubs
      String
      String
      (List ModuleCLabel)
      (List ModuleCLabel)
      (List (StubDecl' idOcc))
  deriving Generic

data Module' idBnd idOcc dcOcc tcBnd tcOcc
  = MkModule
      String
      UnitId
      ModuleName
      (Maybe StgName)
      (ForeignStubs' idOcc)
      Bool
      (List (Pair UnitId (List ModuleName)))
      (List (Pair UnitId (List (Pair ModuleName (List idBnd)))))
      (List (Pair UnitId (List (Pair ModuleName (List tcBnd)))))
      (List (TopBinding' idBnd idOcc dcOcc tcOcc))
  deriving Generic

instance ToJSON Unique where toEncoding = genericToEncoding jsonOptions
instance ToJSON UnhelpfulSpanReason where toEncoding = genericToEncoding jsonOptions
instance ToJSON SrcSpan where toEncoding = genericToEncoding jsonOptions
instance ToJSON Tickish where toEncoding = genericToEncoding jsonOptions
instance ToJSON PrimElemRep where toEncoding = genericToEncoding jsonOptions
instance ToJSON PrimRep where toEncoding = genericToEncoding jsonOptions
instance ToJSON StgType where toEncoding = genericToEncoding jsonOptions
instance ToJSON TyConId where toEncoding = genericToEncoding jsonOptions
instance ToJSON DataConId where toEncoding = genericToEncoding jsonOptions
instance ToJSON DataConRep where toEncoding = genericToEncoding jsonOptions
instance ToJSON UnitId where toEncoding = genericToEncoding jsonOptions
instance ToJSON ModuleName where toEncoding = genericToEncoding jsonOptions
instance ToJSON CbvMark where toEncoding = genericToEncoding jsonOptions
instance ToJSON IdDetails where toEncoding = genericToEncoding jsonOptions
instance ToJSON BinderId where toEncoding = genericToEncoding jsonOptions
instance ToJSON Scope where toEncoding = genericToEncoding jsonOptions
instance ToJSON LitNumType where toEncoding = genericToEncoding jsonOptions
instance ToJSON LabelSpec where toEncoding = genericToEncoding jsonOptions
instance ToJSON Lit where toEncoding = genericToEncoding jsonOptions
instance (ToJSON tcOcc) => ToJSON (AltType' tcOcc) where toEncoding = genericToEncoding jsonOptions
instance ToJSON UpdateFlag where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idOcc) => ToJSON (Arg' idOcc) where toEncoding = genericToEncoding jsonOptions
instance (ToJSON dcOcc) => ToJSON (AltCon' dcOcc) where toEncoding = genericToEncoding jsonOptions
instance ToJSON Safety where toEncoding = genericToEncoding jsonOptions
instance ToJSON CCallConv where toEncoding = genericToEncoding jsonOptions
instance ToJSON SourceText where toEncoding = genericToEncoding jsonOptions
instance ToJSON CCallTarget where toEncoding = genericToEncoding jsonOptions
instance ToJSON PrimCall where toEncoding = genericToEncoding jsonOptions
instance ToJSON StgOp where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idBnd, ToJSON idOcc, ToJSON dcOcc, ToJSON tcOcc) => ToJSON (Binding' idBnd idOcc dcOcc tcOcc) where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idBnd, ToJSON idOcc, ToJSON dcOcc, ToJSON tcOcc) => ToJSON (TopBinding' idBnd idOcc dcOcc tcOcc) where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idBnd, ToJSON idOcc, ToJSON dcOcc, ToJSON tcOcc) => ToJSON (Expr' idBnd idOcc dcOcc tcOcc) where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idBnd, ToJSON idOcc, ToJSON dcOcc, ToJSON tcOcc) => ToJSON (Rhs' idBnd idOcc dcOcc tcOcc) where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idBnd, ToJSON idOcc, ToJSON dcOcc, ToJSON tcOcc) => ToJSON (Alt' idBnd idOcc dcOcc tcOcc) where toEncoding = genericToEncoding jsonOptions
instance ToJSON Header where toEncoding = genericToEncoding jsonOptions
instance ToJSON CImportSpec where toEncoding = genericToEncoding jsonOptions
instance ToJSON CExportSpec where toEncoding = genericToEncoding jsonOptions
instance ToJSON ForeignImport where toEncoding = genericToEncoding jsonOptions
instance ToJSON ForeignExport where toEncoding = genericToEncoding jsonOptions
instance ToJSON StubImpl where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idOcc) => ToJSON (StubDecl' idOcc) where toEncoding = genericToEncoding jsonOptions
instance ToJSON ModuleLabelKind where toEncoding = genericToEncoding jsonOptions
instance ToJSON ModuleCLabel where toEncoding = genericToEncoding jsonOptions
instance ToJSON RealSrcSpan where toEncoding = genericToEncoding jsonOptions
instance ToJSON BufSpan where toEncoding = genericToEncoding jsonOptions
instance ToJSON SDataCon where toEncoding = genericToEncoding jsonOptions
instance ToJSON STyCon where toEncoding = genericToEncoding jsonOptions
instance ToJSON SBinder where toEncoding = genericToEncoding jsonOptions
instance ToJSON ForeignCall where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idOcc) => ToJSON (ForeignStubs' idOcc) where toEncoding = genericToEncoding jsonOptions
instance (ToJSON idBnd, ToJSON idOcc, ToJSON dcOcc, ToJSON tcBnd, ToJSON tcOcc) => ToJSON (Module' idBnd idOcc dcOcc tcBnd tcOcc) where toEncoding = genericToEncoding jsonOptions
