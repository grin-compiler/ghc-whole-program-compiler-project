{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stg.Pretty where

import Control.Monad
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer hiding (Alt)
import Control.Monad.State
import Control.Monad.RWS hiding (Alt)
import Data.Foldable
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Map (Map)
import qualified Data.Map as Map

import Stg.Syntax
import Stg.IRLocation

import Data.Ratio
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.Final
import Text.PrettyPrint.Final.Words
import Text.PrettyPrint.Final.Extensions.Environment

---------------------------------------------------------
type SrcPos = (Int, Int)
type SrcRange = (SrcPos, SrcPos)


{-
data ProgramPoint
  = PP_Global
  | PP_Closure    Id          -- closure name
  | PP_Scrutinee  Id          -- qualified scrutinee result name
  | PP_Alt        Id AltCon   -- qualified scrutinee result name, alternative pattern
  | PP_Apply      Int ProgramPoint

data Tickish
  = ProfNote
  | HpcTick
  | Breakpoint
  | SourceNote
    { sourceSpan :: RealSrcSpan
    , sourceName :: Name
    }
  deriving (Eq, Ord, Generic, Show)

Q:
  code execution breakpoint
    code is executed
  data value access breakpoint
    value is referenced
  reference access breakpoint
    binder is referenced
  allocation breakpoint
    binder is allocated


-- stg ir types



declaration
  TopBinding
    StgTopLifted    - none ; expr covers it
    StgTopStringLit - data
  Binding           - allocation? Q: should this be covered by let?
  Arg               - none ; expr covers it
  Alt               - none ; expr covers it
code
  Expr              - code
  Rhs
    StgRhsClosure   - none ; expr covers it
    StgRhsCon       - 
-}

{-
  named expr parents
    - StgCase result binder (scrutinee expr)
    - StgLet/StgLetNoEscape/StgTick + its parent name
    - StgRhsClosure + Binding binder
    - Alt: alt con + StgCase result binder

-}

getStgPoint :: DocM StgPoint
getStgPoint = askEnv >>= \case
  Nothing -> error "missing stg point"
  Just sp -> pure sp
---------------------------------------------------------

env0 :: Monoid fmt => PEnv Int a fmt
env0 = PEnv
  { maxWidth = 80
  , maxRibbon = 60
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = mempty
  , formatAnn = const mempty
  }

state0 :: PState Int ()
state0 = PState
  { curLine = []
  }

{-
data SPEnv
  = SPEnv
  { speParent         :: Maybe StgPoint
  , speBinderName     :: Maybe Name
  , speScrutineeName  :: Maybe Name
  } Maybe StgPoint
-}
type SPEnv = Maybe StgPoint

withStgPoint :: StgPoint -> Doc -> Doc
withStgPoint sp = localEnv (const $ Just sp)

spEnv0 :: SPEnv
spEnv0 = Nothing


-- For plain text pretty printing
newtype DocM a = DocM { unDocM :: EnvT SPEnv (RWST (PEnv Int StgPoint ()) (POut Int StgPoint) (PState Int ()) Maybe) a }
  deriving
    ( Functor, Applicative, Monad, Alternative
    , MonadReader (PEnv Int StgPoint ())
    , MonadWriter (POut Int StgPoint)
    , MonadState (PState Int ())
    , MonadReaderEnv SPEnv
    )

instance MonadPretty Int StgPoint () DocM
instance MonadPrettyEnv SPEnv Int StgPoint () DocM

instance IsString (DocM ()) where
  fromString = text . fromString

runDocM :: PEnv Int StgPoint () -> SPEnv -> PState Int () -> DocM a -> Maybe (PState Int (), POut Int StgPoint, a)
runDocM e spe s d = (\(a,s',o) -> (s',o,a)) <$> runRWST (runEnvT spe $ unDocM d) e s

execDoc :: Doc -> POut Int StgPoint
execDoc d =
  let rM = runDocM env0 spEnv0 state0 d
  in case rM of
    Nothing -> PAtom $ AChunk $ CText "<internal pretty printing error>"
    Just (_, o, ()) -> o

type Doc = DocM ()

instance Semigroup Doc where
  (<>) = (>>)

instance Monoid Doc where
  mempty = return ()

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty = id

instance Pretty Int where
  pretty = text . T.pack . show

instance Pretty Integer where
  pretty = text . T.pack . show

instance Measure Int () DocM where
  measure = return . runIdentity . measure

instance Pretty Text where
  pretty = text . T.pack . show

instance Pretty String where
  pretty = text . T.pack

instance Pretty Name where
  pretty = text . T.pack . BS.unpack

---------------------------------------------------------

red :: Doc -> Doc
red = id

green :: Doc -> Doc
green = id

angles :: (MonadPretty w ann fmt m) => m () -> m ()
angles x = char '<' >> x >> char '>'

(<+>) :: MonadPretty w ann fmt m => m a -> m b -> m b
a <+> b = a >> char ' ' >> b

x <$$> y = x <> newline <> y

line = newline
hang i d = align (nest i d)
indent i d = hang i (textS (spaces i) <> d)

vcat :: [Doc] -> Doc
vcat = vsep

sep = grouped . vsep

spaces :: Int -> String
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '


textS = text . T.pack
textBS = text . T.pack . BS.unpack

--------------------------------------------------------

replaceNewlineBS :: ByteString -> ByteString
replaceNewlineBS = BS.map f where
  f = \case
    '\r' -> ' '
    '\n' -> ' '
    c    -> c

{-
data ForeignCall
data PrimCall = PrimCall -- Name Name
data UpdateFlag = ReEntrant | Updatable | SingleEntry
-}

smallRArrow :: Doc
smallRArrow = "->"

maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id

ppType :: Type -> Doc
ppType t = red $ case t of
  SingleValue r   -> ppPrimRep r
  UnboxedTuple l  -> braces $ hsep (map ppPrimRep l)
  PolymorphicRep  -> text "PolymorphicRep"

ppPrimRep :: PrimRep -> Doc
ppPrimRep = \case
  VecRep i r  -> angles $ pretty i <+> comma <+> textS (show r)
  r -> textS $ show r

colorBinderExport :: Binder -> Doc -> Doc
colorBinderExport b = case binderScope b of
  ClosurePrivate  -> id
  ModulePrivate   -> id
  ModulePublic    -> green


pprBinder :: Binder -> Doc
pprBinder b = parens $
  pprVar b <+> text ":" <+>
  ppType (binderType b) <+>
  parens (pretty $ replaceNewlineBS $ binderTypeSig b) <+>
  parens (pretty $ show $ binderDetails b)
    where
      BinderId u  = binderId b

pprVar :: Binder -> Doc
pprVar b@Binder{..}
  | binderScope == ModulePublic
  = colorBinderExport b . pretty $ binderUniqueName
  | otherwise
  = colorBinderExport b . pretty $ binderName <> BS.pack ('_' : show u)
  where BinderId u  = binderId

instance Pretty Type where
    pretty = ppType

instance Pretty UnitId where
    pretty = text . T.pack . BS.unpack . getUnitId

instance Pretty ModuleName where
    pretty = text . T.pack . BS.unpack . getModuleName

pprRational :: Rational -> Doc
pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance Pretty LitNumType where
  pretty = \case
    LitNumInt     -> "Int"
    LitNumInt8    -> "Int8"
    LitNumInt16   -> "Int16"
    LitNumInt32   -> "Int32"
    LitNumInt64   -> "Int64"
    LitNumWord    -> "Word"
    LitNumWord8   -> "Word8"
    LitNumWord16  -> "Word16"
    LitNumWord32  -> "Word32"
    LitNumWord64  -> "Word64"

instance Pretty Lit where
    pretty (LitChar x) = "'" <> char x <> "'#"
    pretty (LitString x) = "\"" <> textBS x <> "\"#"
    pretty LitNullAddr = "nullAddr#"
    pretty (LitFloat x) = "FLOAT" <> parens (pprRational x)
    pretty (LitDouble x) = "DOUBLE" <> parens (pprRational x)
    pretty (LitLabel x s) = "LABEL"<> parens (pretty x) <+> textS (show s)
    pretty (LitNumber t i) = "#" <> pretty t <> "#" <> pretty i
    pretty (LitRubbish t) = text "#Rubbish" <+> pretty t

instance Pretty AltCon where
    pretty (AltDataCon dc) = pretty dc
    pretty (AltLit l) = pretty l
    pretty AltDefault = text "DEFAULT"

instance Pretty AltType where
    pretty = \case
      PolyAlt       -> text "PolyAlt"
      MultiValAlt i -> text "MultiValAlt" <+> pretty i
      PrimAlt r     -> text "PrimAlt" <+> ppPrimRep r
      AlgAlt tc     -> text "AlgAlt" <+> ppTyConName tc

instance Pretty Binder where
    pretty = pprBinder


pprExpr :: Expr -> Doc
pprExpr = pprExpr' False

pprAlt :: Name -> Int -> Alt -> Doc
pprAlt scrutName idx (Alt con bndrs rhs) =
  (hsep (pretty con : map (pprBinder) bndrs) <+> smallRArrow) <$$>
  indent 2 (withStgPoint (SP_AltExpr scrutName idx) $ pprExpr' False rhs)

pprArg :: Arg -> Doc
pprArg = \case
  StgVarArg o -> pprVar o
  StgLitArg l -> pretty l

instance Pretty Safety where
  pretty = textS . show

instance Pretty CCallConv where
  pretty = textS . show

instance Pretty CCallTarget where
  pretty = textS . show

instance Pretty ForeignCall where
  pretty ForeignCall{..} = braces $ hsep [pretty foreignCSafety, pretty foreignCConv, pretty foreignCTarget]

instance Pretty PrimCall where
  pretty (PrimCall lbl uid) = braces $ hsep [pretty uid, pretty lbl]

pprOp :: StgOp -> Doc
pprOp = \case
  StgPrimOp op    -> text "_stg_prim_" <> pretty op
  StgPrimCallOp p -> text "_stg_prim_call" <> pretty p
  StgFCallOp f    -> text "_stg_foreign_call" <+> pretty f

pprExpr' :: Bool -> Expr -> Doc
pprExpr' hasParens exp = do
  stgPoint <- getStgPoint
  annotate stgPoint $ case exp of
    StgLit l            -> pretty l
    StgCase x b at alts -> maybeParens hasParens
                           $ sep [ hsep [ text "case"
                                        , withStgPoint (SP_CaseScrutineeExpr $ binderUniqueName b) $
                                          pprExpr' False x
                                        , text "of"
                                        , pprBinder b
                                        , text ":"
                                        , parens (pretty at)
                                        , text "{" ]
                                 , indent 2 $ vcat $ [pprAlt (binderUniqueName b) idx a | (idx, a) <- zip [0..] alts]
                                 , "}"
                                 ]
    StgApp f args         -> maybeParens hasParens $ (pprVar f) <+> (hsep $ map (pprArg) args)
    StgOpApp op args ty n -> maybeParens hasParens $ (pprOp op) <+> (hsep $ map (pprArg) args) <+> text "::" <+> (pretty ty) <+> maybe mempty (parens . ppTyConName) n
    StgConApp dc args _t  -> maybeParens hasParens $ (pretty dc) <+> (hsep $ map (pprArg) args)
    StgLet b e            -> maybeParens hasParens $ text "let" <+> (align $ pprBinding b) <$$> text "in" <+> align (withStgPoint (SP_LetExpr stgPoint) $ pprExpr' False e)
    StgLetNoEscape b e    -> maybeParens hasParens $ text "lettail" <+> (align $ pprBinding b) <$$> text "in" <+> align (withStgPoint (SP_LetNoEscapeExpr stgPoint) $ pprExpr' False e)
    StgTick tickish e     -> pprExpr' hasParens e

instance Pretty Expr where
  pretty = pprExpr

pprRhs :: Name -> Rhs -> Doc
pprRhs rhsBinderName = \case
  StgRhsClosure _ u bs e -> text "\\closure" <+> hsep (map pprBinder bs) <+> text "->" <+> braces (line <> (withStgPoint (SP_RhsClosureExpr rhsBinderName) $ pprExpr e))
  StgRhsCon d vs -> annotate (SP_RhsCon rhsBinderName) $ pretty d <+> (hsep $ map (pprArg) vs)

pprBinding :: Binding -> Doc
pprBinding = \case
  StgNonRec b r  -> pprTopBind (b,r)
  StgRec bs      -> text "rec" <+> braces (line <> vsep (map pprTopBind bs))
  where
    pprTopBind (b,rhs) =
      (pprBinder b <+> equals <$$> (indent 2 $ pprRhs (binderUniqueName b) rhs))
      <> line

pprTopBinding :: TopBinding -> Doc
pprTopBinding = \case
  StgTopLifted (StgNonRec b r)  -> pprTopBind (b,r)
  StgTopLifted (StgRec bs)      -> text "rec" <+> braces (line <> vsep (map pprTopBind bs))
  StgTopStringLit b s           -> pprTopBind' (const $ textS . show) (b,s)
  where
    pprTopBind = pprTopBind' pprRhs
    pprTopBind' f (b,rhs) =
      (pprBinder b <+> equals <$$> (indent 2 $ f (binderUniqueName b) rhs))
      <> line

instance Pretty TopBinding where
  pretty = pprTopBinding

ppTyConName :: TyCon -> Doc
ppTyConName TyCon{..} = pretty tcUnitId <> text "_" <> pretty tcModule <> text "." <> pretty tcName

pprTyCon :: TyCon -> Doc
pprTyCon TyCon{..} = pretty tcUnitId <> text "_" <> pretty tcModule <> text "." <> pretty tcName <$$> (indent 2 $ vsep (map pretty tcDataCons)) <> line where

pprDataCon :: DataCon -> Doc
pprDataCon DataCon{..} = pretty dcUnitId <> text "_" <> pretty dcModule <> text "." <> pretty dcName <+> text "::" <+> textS (show dcRep) <+> parens (textS (show dcId))

instance Pretty DataCon where
    pretty = pprDataCon

pprModule :: Module -> Doc
pprModule m =
  comment (pretty $ modulePhase m)
  <$$> text "package" <+> pretty (moduleUnitId m)
  <$$> text "module" <+> pretty (moduleName m) <+> "where" <> line

  <$$> vsep [text "using" <+> pretty u <+> text ":" <+> pretty mod | (u, ml) <- moduleDependency m, mod <- ml] <> line

  <$$> text "externals" <$$> vsep [indent 2 $ vsep (map pprBinder bl) | (_, ml) <- moduleExternalTopIds m, (_, bl) <- ml] <> line

  <$$> text "type" <$$> vsep [indent 2 $ vsep (map pprTyCon tl) | (_, ml) <- moduleTyCons m, (_, tl) <- ml] <> line

  <$$> vsep (map (pprTopBinding) (moduleTopBindings m))

--  <$$> pprForeignStubs (moduleForeignStubs m)

instance Pretty Module where
  pretty = pprModule

pprForeignStubs :: ForeignStubs -> Doc
pprForeignStubs = \case
  NoStubs           -> empty
  ForeignStubs{..}  -> vsep
                        [ text "foreign stub C header {" <$$> green (pretty fsCHeader) <$$> text "}"
                        , text "foreign stub C source {" <$$> green (pretty fsCSource) <$$> text "}"
                        , text "foreign decls {" <$$> (indent 2 $ vsep $ map (textS . show) fsDecls) <$$> text "}"
                        ]

comment :: Doc -> Doc
comment x = text "{-" <+> x <+> text "-}"

-------------------
-- rendering

data StgPointState
  = StgPointState
  { spsRow        :: Int
  , spsCol        :: Int
  , spsStgPoints  :: [(StgPoint, SrcRange)]
  , spsOutput     :: [Text]
  }
emptyStgPointState :: StgPointState
emptyStgPointState = StgPointState
  { spsRow        = 1
  , spsCol        = 1
  , spsStgPoints  = mempty
  , spsOutput     = mempty
  }

type M = State StgPointState

-- check if there is no new line in text
replaceNewlines :: Text -> Text
replaceNewlines = T.map $
  \case
    '\n' -> ' '
    '\r' -> ' '
    c    -> c

addOutput :: Text -> M ()
addOutput t = modify' $ \s@StgPointState{..} -> s
  { spsCol    = spsCol + T.length t
  , spsOutput = t : spsOutput
  }

addStgPoint :: StgPoint -> SrcRange -> M ()
addStgPoint p r = modify' $ \s@StgPointState{..} -> s { spsStgPoints = (p, r) : spsStgPoints }

getPos :: M SrcPos
getPos = (,) <$> gets spsRow <*> gets spsCol

pShow :: Doc -> (Text, [(StgPoint, SrcRange)])
pShow doc = (T.concat . reverse $ spsOutput result, spsStgPoints result)
  where
    result = execState (renderPOut $ execDoc doc) emptyStgPointState

    renderChunk :: Chunk Int -> M ()
    renderChunk = \case
      CText t   -> addOutput $ replaceNewlines t
      CSpace w  -> addOutput . T.pack $ replicate w ' '

    renderAtom :: Atom Int -> M ()
    renderAtom = \case
      AChunk c  -> renderChunk c
      ANewline  -> do
        addOutput "\n"
        modify' $ \s@StgPointState{..} -> s {spsRow = succ spsRow, spsCol = 1}

    renderPOut :: POut Int StgPoint -> M ()
    renderPOut = \case
      PNull      -> pure ()
      PAtom a    -> renderAtom a
      PSeq o1 o2 -> renderPOut o1 >> renderPOut o2
      PAnn a o   -> do
        start <- getPos
        renderPOut o
        end <- getPos
        addStgPoint a (start, end)

pShowS :: Doc -> (String, [(StgPoint, SrcRange)])
pShowS doc = (T.unpack t, l)
  where (t, l) = pShow doc
