{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Syntax where

import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Binary

-- utility

-- Binder
newtype Id = Id {unId :: Binder}

instance Eq Id where
  (Id a) == (Id b) = binderUNameHash a == binderUNameHash b && binderUniqueName a == binderUniqueName b

instance Ord Id where
  compare (Id a) (Id b) = case compare (binderUNameHash a) (binderUNameHash b) of
    EQ  -> compare (binderUniqueName a) (binderUniqueName b)
    x   -> x

instance Show Id where
  show (Id a) = BS8.unpack $ binderUniqueName a

-- DataCon
newtype DC = DC {unDC :: DataCon}

instance Eq DC where
  (DC a) == (DC b) = dcUNameHash a == dcUNameHash b && dcUniqueName a == dcUniqueName b

instance Ord DC where
  compare (DC a) (DC b) = case compare (dcUNameHash a) (dcUNameHash b) of
    EQ  -> compare (dcUniqueName a) (dcUniqueName b)
    x   -> x

instance Show DC where
  show (DC a) = BS8.unpack $ dcUniqueName a

-- TyCon
newtype TC = TC {unTC :: TyCon}

instance Eq TC where
  (TC a) == (TC b) = tcUNameHash a == tcUNameHash b && tcUniqueName a == tcUniqueName b

instance Ord TC where
  compare (TC a) (TC b) = case compare (tcUNameHash a) (tcUNameHash b) of
    EQ  -> compare (tcUniqueName a) (tcUniqueName b)
    x   -> x

instance Show TC where
  show (TC a) = BS8.unpack $ tcUniqueName a

-- idinfo

type IdInfo = BS8.ByteString

-- data types

type Name = BS8.ByteString

data Unique
  = Unique !Char !Int
  deriving (Eq, Ord, Generic)

instance Show Unique where
 show (Unique c n) = c : iToBase62 n

iToBase62 :: Int -> String
iToBase62 n_ = go n_ "" where
  go n cs | n < 62
          = let c = chooseChar62 n in c : cs
          | otherwise
          = go q (c1 : cs) where (q, r) = quotRem n 62
                                 c1 = chooseChar62 r

  chooseChar62 :: Int -> Char
  chooseChar62 n = chars62 !! n
  chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- source location related

data RealSrcSpan
  = RealSrcSpan'
  { srcSpanFile   :: !Name
  , srcSpanSLine  :: !Int
  , srcSpanSCol   :: !Int
  , srcSpanELine  :: !Int
  , srcSpanECol   :: !Int
  }
  deriving (Eq, Ord, Generic, Show)

data BufSpan
  = BufSpan
  { bufSpanStart  :: !Int
  , bufSpanEnd    :: !Int
  }
  deriving (Eq, Ord, Generic, Show)

data SrcSpan
  = RealSrcSpan   !RealSrcSpan !(Maybe BufSpan)
  | UnhelpfulSpan !UnhelpfulSpanReason
  deriving (Eq, Ord, Generic, Show)

data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulWiredIn
  | UnhelpfulInteractive
  | UnhelpfulGenerated
  | UnhelpfulOther !Name
  deriving (Eq, Ord, Generic, Show)

-- tickish related

data Tickish
  = ProfNote
  | HpcTick
  | Breakpoint
  | SourceNote
    { sourceSpan :: RealSrcSpan
    , sourceName :: Name
    }
  deriving (Eq, Ord, Generic, Show)

-- type related

data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep   -- ^ Unlifted pointer
  | Int8Rep       -- ^ Signed, 8-bit value
  | Int16Rep      -- ^ Signed, 16-bit value
  | Int32Rep      -- ^ Signed, 32-bit value
  | Int64Rep      -- ^ Signed, 64 bit value (with 32-bit words only)
  | IntRep        -- ^ Signed, word-sized value
  | Word8Rep      -- ^ Unsigned, 8 bit value
  | Word16Rep     -- ^ Unsigned, 16 bit value
  | Word32Rep     -- ^ Unsigned, 32 bit value
  | Word64Rep     -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | WordRep       -- ^ Unsigned, word-sized value
  | AddrRep       -- ^ A pointer, but /not/ to a Haskell value (use '(Un)liftedRep')
  | FloatRep
  | DoubleRep
  | VecRep Int PrimElemRep  -- ^ A vector
  deriving (Eq, Ord, Generic, Show)

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
  deriving (Eq, Ord, Generic, Show)


{-
  Q: do we want to keep haskell types OR would representation type system be enough?
  A: keep only those information that is relevant for the codegen

  HINT: extrenal STG and lambda IR should be identical
-}

data Type
  = SingleValue     !PrimRep
  | UnboxedTuple    ![PrimRep]
  | PolymorphicRep
  deriving (Eq, Ord, Generic, Show)

-- data con related

newtype TyConId
  = TyConId Unique
  deriving (Eq, Ord, Binary, Generic, Show)

newtype DataConId
  = DataConId Unique
  deriving (Eq, Ord, Binary, Generic, Show)

-- raw data con
data DataConRep
  = AlgDataCon      ![PrimRep]
  | UnboxedTupleCon !Int
  deriving (Eq, Ord, Generic, Show)

data SDataCon
  = SDataCon
  { sdcName   :: !Name
  , sdcId     :: !DataConId
  , sdcRep    :: !DataConRep
  , sdcWorker :: !SBinder
  , sdcDefLoc :: !SrcSpan
  }
  deriving (Eq, Ord, Generic, Show)

data STyCon
  = STyCon
  { stcName     :: !Name
  , stcId       :: !TyConId
  , stcDataCons :: ![SDataCon]
  , stcDefLoc   :: !SrcSpan
  }
  deriving (Eq, Ord, Generic, Show)

newtype CutTyCon = CutTyCon {uncutTyCon :: TyCon }
instance Eq CutTyCon where _ == _ = True
instance Ord CutTyCon where compare _ _ = EQ
instance Show CutTyCon where show (CutTyCon tc) = "CutTyCon " ++ (BS8.unpack $ tcUniqueName tc)

-- user friendly data con
data DataCon
  = DataCon
  { dcName   :: !Name
  , dcId     :: !DataConId
  , dcUnitId :: !UnitId
  , dcModule :: !ModuleName
  , dcRep    :: !DataConRep
  , dcTyCon  :: !CutTyCon
  , dcWorker :: !Binder
  , dcDefLoc :: !SrcSpan
  -- optimization
  , dcUniqueName  :: {-# UNPACK #-} !Name
  , dcUNameHash   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Generic, Show)

data TyCon
  = TyCon
  { tcName      :: !Name
  , tcId        :: !TyConId
  , tcUnitId    :: !UnitId
  , tcModule    :: !ModuleName
  , tcDataCons  :: ![DataCon]
  , tcDefLoc    :: !SrcSpan
  -- optimization
  , tcUniqueName  :: {-# UNPACK #-} !Name
  , tcUNameHash   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Generic, Show)

-- id info

data CbvMark
  = MarkedCbv
  | NotMarkedCbv
  deriving (Eq, Ord, Generic, Show)

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
  | JoinId        Int (Maybe [CbvMark])
  | WorkerLikeId  [CbvMark]
  deriving (Eq, Ord, Generic, Show)

-- stg expr related

newtype UnitId
  = UnitId Name
  deriving (Eq, Ord, Binary, Generic, Show)

getUnitId :: UnitId -> Name
getUnitId (UnitId n) = n

newtype ModuleName
  = ModuleName Name
  deriving (Eq, Ord, Binary, Generic, Show)

getModuleName :: ModuleName -> Name
getModuleName (ModuleName n) = n

newtype BinderId
  = BinderId Unique
  deriving (Eq, Ord, Binary, Generic, Show)

data SBinder
  = SBinder
    { sbinderName     :: !Name
    , sbinderId       :: !BinderId
    , sbinderType     :: !Type
    , sbinderTypeSig  :: !Name
    , sbinderScope    :: !Scope
    , sbinderDetails  :: !IdDetails
    , sbinderInfo     :: !IdInfo
    , sbinderDefLoc   :: !SrcSpan
    }
  deriving (Eq, Ord, Generic, Show)

data Binder
  = Binder
    { binderName      :: !Name
    , binderId        :: !BinderId
    , binderType      :: !Type
    , binderTypeSig   :: !Name
    , binderScope     :: !Scope
    , binderDetails   :: !IdDetails
    , binderInfo      :: !IdInfo
    , binderDefLoc    :: !SrcSpan
    , binderUnitId    :: !UnitId
    , binderModule    :: !ModuleName
    , binderTopLevel  :: !Bool
    -- optimization
    , binderUniqueName  :: {-# UNPACK #-} !Name
    , binderUNameHash   :: {-# UNPACK #-} !Int
    }
  deriving (Eq, Ord, Generic, Show)

data Scope
  = ModulePublic    -- ^ visible for every haskell module
  | ModulePrivate   -- ^ visible for a single haskell module
  | ClosurePrivate  -- ^ visible for expression body
  deriving (Eq, Ord, Generic, Show)

mkTyConUniqueName :: UnitId -> ModuleName -> STyCon -> Name
mkTyConUniqueName unitId modName STyCon{..} = getUnitId unitId <> "_" <> getModuleName modName <> "." <> stcName

mkDataConUniqueName :: UnitId -> ModuleName -> SDataCon -> Name
mkDataConUniqueName unitId modName SDataCon{..} = getUnitId unitId <> "_" <> getModuleName modName <> "." <> sdcName

mkBinderUniqueName :: Bool -> UnitId -> ModuleName -> SBinder -> Name
mkBinderUniqueName isTopLevel unitId modName SBinder{..}
 | sbinderId == rootMainBinderId
 = "main_:Main.main"

 | otherwise
 = case sbinderScope of
  ModulePublic    -> getUnitId unitId <> "_" <> getModuleName modName <> "." <> sbinderName
  ModulePrivate   -> getUnitId unitId <> "_" <> getModuleName modName <> "." <> sbinderName <> BS8.pack ('_' : show u)
  ClosurePrivate  -> if isTopLevel || True
                      then getUnitId unitId <> "_" <> getModuleName modName <> "." <> sbinderName <> BS8.pack ('_' : show u)
                      else sbinderName <> BS8.pack ('_' : show u)
  where
    BinderId u = sbinderId

rootMainBinderId :: BinderId
rootMainBinderId = BinderId $ Unique '0' 101

data LitNumType
  = LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt8    -- ^ @Int8#@ - exactly 8 bits
  | LitNumInt16   -- ^ @Int16#@ - exactly 16 bits
  | LitNumInt32   -- ^ @Int32#@ - exactly 32 bits
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord8   -- ^ @Word8#@ - exactly 8 bits
  | LitNumWord16  -- ^ @Word16#@ - exactly 16 bits
  | LitNumWord32  -- ^ @Word32#@ - exactly 32 bits
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Eq, Ord, Generic, Show)

data LabelSpec
  = FunctionLabel !(Maybe Int) -- only for stdcall convention
  | DataLabel
  deriving (Eq, Ord, Generic, Show)

data Lit
  = LitChar     !Char
  | LitString   !BS.ByteString
  | LitNullAddr
  | LitFloat    !Rational
  | LitDouble   !Rational
  | LitLabel    !BS8.ByteString LabelSpec
  | LitNumber   !LitNumType !Integer
  | LitRubbish  !Type
  deriving (Eq, Ord, Generic, Show)

-- | A top-level binding.
data TopBinding' idBnd idOcc dcOcc tcOcc
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted    (Binding' idBnd idOcc dcOcc tcOcc)
  | StgTopStringLit idBnd BS.ByteString
  deriving (Eq, Ord, Generic, Show)

data Binding' idBnd idOcc dcOcc tcOcc
  = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
  | StgRec    [(idBnd, Rhs' idBnd idOcc dcOcc tcOcc)]
  deriving (Eq, Ord, Generic, Show)

data Arg' idOcc
  = StgVarArg  idOcc
  | StgLitArg  !Lit
  deriving (Eq, Ord, Generic, Show)

data Expr' idBnd idOcc dcOcc tcOcc
  = StgApp
        idOcc         -- function
        [Arg' idOcc]  -- arguments; may be empty

  | StgLit      Lit

        -- StgConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound first
  | StgConApp   dcOcc         -- DataCon
                [Arg' idOcc]  -- Saturated
                [Type]        -- types

  | StgOpApp    StgOp         -- Primitive op or foreign call
                [Arg' idOcc]  -- Saturated.
                Type          -- result type
                (Maybe tcOcc) -- result type name (required for tagToEnum wrapper generator)

  | StgCase
        (Expr' idBnd idOcc dcOcc tcOcc)     -- the thing to examine

        idBnd                               -- binds the result of evaluating the scrutinee
        (AltType' tcOcc)
        [Alt' idBnd idOcc dcOcc tcOcc]      -- The DEFAULT case is always *first*
                                            -- if it is there at all

  | StgLet
        (Binding' idBnd idOcc dcOcc tcOcc)  -- right hand sides (see below)
        (Expr' idBnd idOcc dcOcc tcOcc)     -- body

  | StgLetNoEscape
        (Binding' idBnd idOcc dcOcc tcOcc)  -- right hand sides (see below)
        (Expr' idBnd idOcc dcOcc tcOcc)     -- body

  | StgTick
        Tickish
        (Expr' idBnd idOcc dcOcc tcOcc)     -- sub expression
  deriving (Eq, Ord, Generic, Show)

data AltType' tcOcc
  = PolyAlt
  | MultiValAlt !Int
  | PrimAlt     !PrimRep
  | AlgAlt      tcOcc
  deriving (Eq, Ord, Generic, Show)

data UpdateFlag = ReEntrant | Updatable | SingleEntry
  deriving (Eq, Ord, Generic, Show)

data Rhs' idBnd idOcc dcOcc tcOcc
  = StgRhsClosure
        [idOcc]                   -- non-global free vars
        !UpdateFlag               -- ReEntrant | Updatable | SingleEntry
        [idBnd]                   -- arguments; if empty, then not a function;
                                  -- as above, order is important.
        (Expr' idBnd idOcc dcOcc tcOcc) -- body

  | StgRhsCon
        dcOcc               -- DataCon
        [Arg' idOcc]        -- Args
  deriving (Eq, Ord, Generic, Show)

data Alt' idBnd idOcc dcOcc tcOcc
  = Alt
    { altCon     :: !(AltCon' dcOcc)
    , altBinders :: [idBnd]
    , altRHS     :: Expr' idBnd idOcc dcOcc tcOcc
    }
  deriving (Eq, Ord, Generic, Show)

data AltCon' dcOcc
  = AltDataCon  dcOcc
  | AltLit      !Lit
  | AltDefault
  deriving (Eq, Ord, Generic, Show)

data Safety = PlaySafe | PlayInterruptible | PlayRisky
  deriving (Eq, Ord, Generic, Show)

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
  deriving (Eq, Ord, Generic, Show)

data SourceText
  = SourceText    !BS8.ByteString
  | NoSourceText
  deriving (Eq, Ord, Generic, Show)

data CCallTarget
  = StaticTarget !SourceText !BS8.ByteString !(Maybe UnitId) !Bool {- is function -}
  | DynamicTarget
  deriving (Eq, Ord, Generic, Show)

data ForeignCall
  = ForeignCall
  { foreignCTarget  :: !CCallTarget
  , foreignCConv    :: !CCallConv
  , foreignCSafety  :: !Safety
  }
  deriving (Eq, Ord, Generic, Show)

data PrimCall = PrimCall !BS8.ByteString !UnitId
  deriving (Eq, Ord, Generic, Show)

data StgOp
  = StgPrimOp     !Name
  | StgPrimCallOp !PrimCall
  | StgFCallOp    !ForeignCall
  deriving (Eq, Ord, Generic, Show)

-- foreign export stubs
data Header = Header !SourceText !Name
  deriving (Eq, Ord, Generic, Show)

data ForeignImport = CImport !CCallConv !Safety !(Maybe Header) !CImportSpec !SourceText
  deriving (Eq, Ord, Generic, Show)

data CImportSpec
  = CLabel    !Name
  | CFunction !CCallTarget
  | CWrapper
  deriving (Eq, Ord, Generic, Show)

data ForeignExport = CExport !CExportSpec !SourceText
  deriving (Eq, Ord, Generic, Show)

data CExportSpec = CExportStatic !SourceText !Name !CCallConv
  deriving (Eq, Ord, Generic, Show)

data StubImpl
  = StubImplImportCWrapper  !Name !(Maybe Int) !Bool !Name ![Name]
  | StubImplImportCApi      !Name ![(Maybe Header, BS8.ByteString, Char)]
  deriving (Eq, Ord, Generic, Show)

data StubDecl' idOcc
  = StubDeclImport !ForeignImport !(Maybe StubImpl)
  | StubDeclExport !ForeignExport idOcc !BS8.ByteString
  deriving (Eq, Ord, Generic, Show)

data ModuleLabelKind
    = MLK_Initializer       Name
    | MLK_InitializerArray
    | MLK_Finalizer         Name
    | MLK_FinalizerArray
    | MLK_IPEBuffer
  deriving (Eq, Ord, Generic, Show)

data ModuleCLabel
  = ModuleCLabel !UnitId !ModuleName !ModuleLabelKind
  deriving (Eq, Ord, Generic, Show)

data ForeignStubs' idOcc
  = NoStubs
  | ForeignStubs
    { fsCHeader       :: !BS8.ByteString
    , fsCSource       :: !BS8.ByteString
    , fsInitializers  :: ![ModuleCLabel]
    , fsFinalizers    :: ![ModuleCLabel]
    , fsDecls         :: ![StubDecl' idOcc]
    }
  deriving (Eq, Ord, Generic, Show)

-- the whole module

data Module' idBnd idOcc dcOcc tcBnd tcOcc
  = Module
  { modulePhase               :: !BS8.ByteString
  , moduleUnitId              :: !UnitId
  , moduleName                :: !ModuleName
  , moduleSourceFilePath      :: !(Maybe Name) -- HINT: RealSrcSpan's source file refers to this value
  , moduleForeignStubs        :: !(ForeignStubs' idOcc)
  , moduleHasForeignExported  :: !Bool
  , moduleDependency          :: ![(UnitId, [ModuleName])]
  , moduleExternalTopIds      :: ![(UnitId, [(ModuleName, [idBnd])])]
  , moduleTyCons              :: ![(UnitId, [(ModuleName, [tcBnd])])]
  , moduleTopBindings         :: ![TopBinding' idBnd idOcc dcOcc tcOcc]
  }
  deriving (Eq, Ord, Generic, Show)

-- convenience layers: raw and user friendly

-- raw - as it is serialized
type SModule        = Module'       SBinder BinderId DataConId STyCon  TyConId
type STopBinding    = TopBinding'   SBinder BinderId DataConId TyConId
type SBinding       = Binding'      SBinder BinderId DataConId TyConId
type SExpr          = Expr'         SBinder BinderId DataConId TyConId
type SRhs           = Rhs'          SBinder BinderId DataConId TyConId
type SAlt           = Alt'          SBinder BinderId DataConId TyConId
type SAltCon        = AltCon'       DataConId
type SAltType       = AltType'      TyConId
type SArg           = Arg'          BinderId
type SStubDecl      = StubDecl'     BinderId
type SForeignStubs  = ForeignStubs' BinderId

-- user friendly - rich information
type Module       = Module'       Binder Binder DataCon TyCon TyCon
type TopBinding   = TopBinding'   Binder Binder DataCon TyCon
type Binding      = Binding'      Binder Binder DataCon TyCon
type Expr         = Expr'         Binder Binder DataCon TyCon
type Rhs          = Rhs'          Binder Binder DataCon TyCon
type Alt          = Alt'          Binder Binder DataCon TyCon
type AltCon       = AltCon'       DataCon
type AltType      = AltType'      TyCon
type Arg          = Arg'          Binder
type StubDecl     = StubDecl'     Binder
type ForeignStubs = ForeignStubs' Binder

instance Binary Unique
instance Binary PrimElemRep
instance Binary PrimRep
instance Binary Type
instance Binary CbvMark
instance Binary IdDetails
instance Binary Scope
instance Binary Binder
instance Binary SBinder
instance Binary LitNumType
instance Binary LabelSpec
instance Binary Lit
instance Binary SourceText
instance Binary CCallTarget
instance Binary CCallConv
instance Binary Safety
instance Binary ForeignCall
instance Binary PrimCall
instance Binary UpdateFlag
instance Binary StgOp
instance Binary DataConRep
instance Binary SDataCon
instance Binary STyCon
instance Binary RealSrcSpan
instance Binary BufSpan
instance Binary UnhelpfulSpanReason
instance Binary SrcSpan
instance Binary Tickish
instance Binary Header
instance Binary CImportSpec
instance Binary CExportSpec
instance Binary ForeignImport
instance Binary ForeignExport
instance Binary StubImpl
instance Binary ModuleLabelKind
instance Binary ModuleCLabel
instance (Binary idOcc) => Binary (StubDecl' idOcc)
instance (Binary idOcc) => Binary (ForeignStubs' idOcc)
instance (Binary tcOcc) => Binary (AltType' tcOcc)
instance (Binary dcOcc) => Binary (AltCon' dcOcc)
instance (Binary idOcc) => Binary (Arg' idOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (TopBinding' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Binding' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Rhs' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Alt' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Expr' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc, Binary tcBnd) => Binary (Module' idBnd idOcc dcOcc tcOcc tcBnd)
