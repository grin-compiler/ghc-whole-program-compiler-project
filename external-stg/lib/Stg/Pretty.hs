{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stg.Pretty where
import           Control.Applicative                           (Alternative (..), Applicative (..), (<$>))
import           Control.Monad                                 (Monad (..))
import           Control.Monad.Identity                        (Identity (..))
import           Control.Monad.Reader                          (MonadReader, ReaderT (ReaderT))
import           Control.Monad.RWS                             (RWST (..))
import           Control.Monad.State                           (MonadState, State, execState, gets, modify')
import           Control.Monad.Writer                          (MonadWriter)

import           Data.Bool                                     (Bool (..), otherwise)
import qualified Data.ByteString.Char8                         as BS
import           Data.Eq                                       (Eq (..))
import           Data.Function                                 (const, id, ($), (.))
import           Data.Functor                                  (Functor (..))
import           Data.Int                                      (Int)
import           Data.List                                     (concatMap, filter, repeat, replicate, reverse, zip,
                                                                (++))
import           Data.Maybe                                    (Maybe (..), fromMaybe)
import           Data.Monoid                                   (Monoid (..))
import           Data.Ord                                      (Ord (..))
import           Data.Ratio                                    (Rational, denominator, numerator)
import           Data.Semigroup                                (Semigroup (..))
import           Data.String                                   (IsString (..), String)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Data.Tuple                                    (fst)

import           GHC.Err                                       (error)
import           GHC.Num                                       (Integer, Num (..))

import           Prelude                                       (Enum (..))

import           Stg.IRLocation                                (StgPoint (..), binderToStgId)
import           Stg.Syntax                                    (Alt, Alt' (..), AltCon, AltCon' (..), AltType,
                                                                AltType' (..), Arg, Arg' (..), Binder (..),
                                                                BinderId (..), Binding, Binding' (..), CCallConv,
                                                                CCallTarget (..), DataCon (..), DataConRep (..), Expr,
                                                                Expr' (..), ForeignCall (..), ForeignStubs,
                                                                ForeignStubs' (..), Id (Id), Lit (..), LitNumType (..),
                                                                Module, Module' (..), ModuleName, Name, PrimCall (..),
                                                                PrimRep (VecRep), RealSrcSpan (..), Rhs, Rhs' (..),
                                                                Safety, Scope (..), StgOp (..), Tickish (..),
                                                                TopBinding, TopBinding' (..), TyCon (..), Type (..),
                                                                UnitId, getModuleName, getUnitId)

import           Text.PrettyPrint.Final                        (Atom (..), Chunk (..), Failure (..), Layout (..), Line,
                                                                Measure (..), MonadPretty, PEnv (..), POut (..),
                                                                PState (..), align, annotate, char, collection, grouped,
                                                                hsep, nest, newline, text, vsep)
import           Text.PrettyPrint.Final.Extensions.Environment (EnvT (..), MonadPrettyEnv, MonadReaderEnv (..), runEnvT)
import           Text.PrettyPrint.Final.Words                  (braces, comma, parens)
import           Text.Show                                     (Show (..))

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
getStgPoint = askEnv >>= (\case
  Nothing -> error "missing stg point"
  Just sp -> pure sp) . speStgPoint
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

newtype Config
  = Config
  { cfgPrintTickish :: Bool
  }

data SPEnv
  = SPEnv
  { speStgPoint :: Maybe StgPoint
  , speConfig   :: Config
  }

withStgPoint :: StgPoint -> Doc -> Doc
withStgPoint sp = localEnv (\env -> env {speStgPoint = Just sp})

spEnv0 :: Config -> SPEnv
spEnv0 cfg = SPEnv
  { speStgPoint = Nothing
  , speConfig   = cfg
  }


-- For plain text pretty printing
newtype DocM a = DocM { unDocM :: EnvT SPEnv (RWST (PEnv Int StgPoint ()) (POut Int StgPoint) (PState Int ()) Maybe) a }
  deriving newtype (Functor, Applicative, Monad, Alternative
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

execDoc :: Config -> Doc -> POut Int StgPoint
execDoc cfg d =
  let rM = runDocM env0 (spEnv0 cfg) state0 d
  in case rM of
    Nothing         -> PAtom $ AChunk $ CText "<internal pretty printing error>"
    Just (_, o, ()) -> o

type Doc = DocM ()

instance Semigroup Doc where
  (<>) :: Doc -> Doc -> Doc
  (<>) = (>>)

instance Monoid Doc where
  mempty :: Doc
  mempty = return ()

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty :: Doc -> Doc
  pretty = id

instance Pretty Int where
  pretty :: Int -> Doc
  pretty = text . T.pack . show

instance Pretty Integer where
  pretty :: Integer -> Doc
  pretty = text . T.pack . show

instance Measure Int () DocM where
  measure :: Line Int () -> DocM Int
  measure = return . runIdentity . measure

instance Pretty Text where
  pretty :: Text -> Doc
  pretty = text . T.pack . show

instance Pretty String where
  pretty :: String -> Doc
  pretty = text . T.pack

instance Pretty Name where
  pretty :: Name -> Doc
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

(<$$>) :: (MonadPretty w ann fmt m, Semigroup (m ())) => m () -> m () -> m ()
x <$$> y = x <> newline <> y

hang :: MonadPretty w ann fmt m => w -> m a -> m a
hang i d = align (nest i d)

indent :: Int -> DocM () -> DocM ()
indent i d = hang i (textS (spaces i) <> d)

vcat :: [Doc] -> Doc
vcat = vsep

sep :: [DocM ()] -> DocM ()
sep = grouped . vsep

spaces :: Int -> String
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '

textS :: String -> DocM ()
textS = text . T.pack

--------------------------------------------------------

ppType :: Type -> Doc
ppType t = red $ case t of
  SingleValue r  -> ppPrimRep r
  UnboxedTuple l -> braces $ hsep (fmap ppPrimRep l)
  PolymorphicRep -> text "PolymorphicRep"

ppPrimRep :: PrimRep -> Doc
ppPrimRep = \case
  VecRep i r  -> angles $ pretty i <+> comma <+> textS (show r)
  r -> textS $ show r

colorBinderExport :: Binder -> Doc -> Doc
colorBinderExport b = case binderScope b of
  ClosurePrivate -> id
  ModulePrivate  -> id
  ModulePublic   -> green

pprBinderTypeSig :: Binder -> Doc
pprBinderTypeSig b = pprVar b <+> text "::" <+> ppType (binderType b)

pprBinder :: Binder -> Doc
pprBinder = pprVar

{-
  name handling design:
  + show normal (binder name for public functions)
  + show only the unique id for module local names, to keep the code short
    the module local names are GHC generated most of the time anyway
    the binder name could be displayed in the hover
-}
pprVar :: Binder -> Doc
pprVar b@Binder{..}
  | binderScope == ModulePublic
  = colorBinderExport b . pretty $ binderName
  | otherwise
--  = colorBinderExport b . pretty $ binderName <> BS.pack ('_' : show u)
  = colorBinderExport b . pretty $ BS.pack (show u)
  where
    BinderId u = binderId

instance Pretty Type where
    pretty :: Type -> Doc
    pretty = ppType

instance Pretty UnitId where
    pretty :: UnitId -> Doc
    pretty = text . T.pack . BS.unpack . getUnitId

instance Pretty ModuleName where
    pretty :: ModuleName -> Doc
    pretty = text . T.pack . BS.unpack . getModuleName

pprRational :: Rational -> Doc
pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance Pretty LitNumType where
  pretty :: LitNumType -> Doc
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
    pretty :: Lit -> Doc
    pretty (LitChar x)      = text (T.pack $ show x)
    pretty (LitString x)    = text (T.pack $ show x)
    pretty LitNullAddr      = "nullAddr#"
    pretty (LitFloat x)     = pprRational x
    pretty (LitDouble x)    = pprRational x
    pretty (LitLabel x s)   = text "LABEL" <+> parens (pretty x) <+> textS (show s)
    pretty (LitNumber _t i) = pretty i
    pretty (LitRubbish t)   = text "#Rubbish" <+> pretty t

instance Pretty AltCon where
    pretty :: AltCon -> Doc
    pretty (AltDataCon dc) = pprDataConName dc
    pretty (AltLit l)      = pretty l
    pretty AltDefault      = text "_"

instance Pretty AltType where
    pretty :: AltType -> Doc
    pretty = \case
      PolyAlt       -> text "PolyAlt"
      MultiValAlt i -> text "MultiValAlt" <+> pretty i
      PrimAlt r     -> text "PrimAlt" <+> ppPrimRep r
      AlgAlt tc     -> text "AlgAlt" <+> ppTyConName tc

pprAlt :: Id -> Int -> Alt -> Doc
pprAlt (Id scrutBinder) idx (Alt con bndrs rhs) =
  (hsep (pretty con : fmap pprBinder bndrs) <+> text "-> do") <$$>
  indent 2 (withStgPoint (SP_AltExpr (binderToStgId scrutBinder) idx) $ pprExpr rhs)

pprArg :: Arg -> Doc
pprArg = \case
  StgVarArg o -> pprVar o
  StgLitArg l -> pretty l

instance Pretty Safety where
  pretty :: Safety -> Doc
  pretty = textS . show

instance Pretty CCallConv where
  pretty :: CCallConv -> Doc
  pretty = textS . show

instance Pretty CCallTarget where
  pretty :: CCallTarget -> Doc
  pretty = textS . show

instance Pretty ForeignCall where
  pretty :: ForeignCall -> Doc
  pretty ForeignCall{..} = braces $ hsep [pretty foreignCSafety, pretty foreignCConv, pretty foreignCTarget]

instance Pretty PrimCall where
  pretty :: PrimCall -> Doc
  pretty (PrimCall lbl uid) = braces $ hsep [pretty uid, pretty lbl]

pprOp :: StgOp -> Doc
pprOp = \case
  StgPrimOp op -> text "primop" <+> pretty (show op)
  StgPrimCallOp (PrimCall sym _uid) -> text "cmmcall" <+> pretty (show sym)-- <+> text "-- from package:" <+> pretty uid
  StgFCallOp ForeignCall{..} -> case foreignCTarget of
    StaticTarget _ sym _ _ -> text "foreigncall" <+> pretty (show sym)
    DynamicTarget          -> text "foreigncall dynamic_call_target"

{-
  - put infix names to parenthesis
  done - do not use fully qualified names
  done - omit parenthesis from: sat_srv @ (_)
  - show in hower:
      + type
      + unit
      + module
      + id details

  - add jump to definition where possible
      + display first few lines of definition

  done - show type signatures for top level binders

  done - use import "network" Network.Socket
    done + use import list

  done - use normal let syntax for let no escape, add comment that it is stack allocated
  done - use haskell data definition syntax
  done - show only the module's data type definitons
  done - add comment for stack allocated unboxed tuples: -- stack allocated
  done - simple printer for foreign calls
  - do not show dead binders
  - include used data constructors in import lists
  - include local data constructor in export lists?
-}

putDefaultLast :: [Alt] -> [Doc] -> [Doc]
putDefaultLast (Alt AltDefault _ _ : _) (first : rest) = rest ++ [first]
putDefaultLast _ l                                     = l

pprRealSrcSpan :: RealSrcSpan -> Doc
pprRealSrcSpan RealSrcSpan'{..} = pretty srcSpanFile <+> pprPos srcSpanSLine srcSpanSCol <> text "-" <> pprPos srcSpanELine srcSpanECol
  where pprPos line col = parens $ pretty line <> text ":" <> pretty col

instance Pretty RealSrcSpan where
  pretty :: RealSrcSpan -> Doc
  pretty = pprRealSrcSpan

pprTickish :: Tickish -> Doc
pprTickish = \case
  ProfNote        -> text "-- ProfNote"
  HpcTick         -> text "-- HpcTick"
  Breakpoint      -> text "-- Breakpoint"
  SourceNote{..}  -> text "-- SourceNote for" <+> pretty sourceName <+> pretty sourceSpan

instance Pretty Tickish where
  pretty :: Tickish -> Doc
  pretty = pprTickish

pprExpr :: Expr -> Doc
pprExpr exp = do
  stgPoint <- getStgPoint
  annotate stgPoint $ case exp of
    StgLit l            -> pretty l
    StgCase x b _at [Alt AltDefault [] rhs] -> sep
                            [ withStgPoint (SP_CaseScrutineeExpr $ binderToStgId b) $
                                pprBinder b <+> text "<-" <+> nest 2 (pprExpr x)
                            , withStgPoint (SP_AltExpr (binderToStgId b) 0) $
                                pprExpr rhs
                            ]
    StgCase x b _at [Alt con bndrs rhs] -> sep
                            [ withStgPoint (SP_CaseScrutineeExpr $ binderToStgId b) $
                                pprBinder b <+> text "@" <+> parens (hsep $ pretty con : fmap pprBinder bndrs) <+> text "<-" <+> nest 2 (pprExpr x)
                            , withStgPoint (SP_AltExpr (binderToStgId b) 0) $
                                pprExpr rhs
                            ]
    StgCase x b _at alts -> sep
                            [ withStgPoint (SP_CaseScrutineeExpr $ binderToStgId b) $
                                pprBinder b <+> text "<-" <+> nest 2 (pprExpr x)
                            , text "case" <+> pprVar b <+> text "of"
                            , indent 2 $ vcat $ putDefaultLast alts [pprAlt (Id b) idx a | (idx, a) <- zip [0..] alts]
                            ]
    StgApp f args         -> pprVar f <+> hsep (fmap pprArg args)
    StgOpApp op args _ty _n -> pprOp op <+> hsep (fmap pprArg args){- <+> text "::" <+> (pretty ty) <+> maybe mempty (parens . ppTyConName) n -}
    StgConApp dc args _t  -> addUnboxedCommentIfNecessary dc $ pprDataConName dc <+> hsep (fmap pprArg args)
    StgLet b e            -> text "let" <+> align (pprBinding b) <$$> align (withStgPoint (SP_LetExpr stgPoint) $ pprExpr e)
    StgLetNoEscape b e    -> vsep
      [ text "-- stack allocating let"
      , text "let" <+> align (pprBinding b) <$$> align (withStgPoint (SP_LetNoEscapeExpr stgPoint) $ pprExpr e)
      ]
    StgTick tickish e -> do
      Config{..} <- speConfig <$> askEnv
      if cfgPrintTickish
        then vsep [annotate (SP_Tickish stgPoint) $ pretty tickish, pprExpr e]
        else pprExpr e

instance Pretty Expr where
  pretty :: Expr -> Doc
  pretty = pprExpr

addUnboxedCommentIfNecessary :: DataCon -> Doc -> Doc
addUnboxedCommentIfNecessary DataCon{..} doc = case dcRep of
  UnboxedTupleCon{} -> doc -- vsep [text "-- stack allocated unboxed tuple", doc]
  _                 -> doc
{-
pprSrcSpan :: SrcSpan -> Doc
pprSrcSpan = \case
  UnhelpfulSpan UnhelpfulNoLocationInfo -> mempty
  UnhelpfulSpan (UnhelpfulOther s)      -> text "-- src-loc:" <+> pretty s
  UnhelpfulSpan sr                      -> text "-- src-loc:" <+> text (T.pack $ show sr)
  RealSrcSpan sp _                      -> text "-- src-loc:" <+> pretty sp
-}
pprRhs :: Id -> Rhs -> Doc
pprRhs (Id rhsBinder) = \case
  StgRhsClosure _ _ bs e ->
    annotate (SP_Binding $ binderToStgId rhsBinder) $
      pprBinder rhsBinder <+> hsep (fmap pprBinder bs) <+> text "= do"
        <> newline
        <> indent 2 (withStgPoint (SP_RhsClosureExpr $ binderToStgId rhsBinder) $ pprExpr e)
  StgRhsCon dc vs ->
    annotate (SP_RhsCon $ binderToStgId rhsBinder) $
      pprBinder rhsBinder
        <+> text "="
        <+> addUnboxedCommentIfNecessary dc (pprDataConName dc <+> hsep (fmap pprArg vs))

pprBinding :: Binding -> Doc
pprBinding = \case
  StgNonRec b r  -> pprBind (b,r)
  StgRec bs      -> vsep (fmap pprBind bs)
  where
    pprBind (b,rhs) =
      pprRhs (Id b) rhs

pprTopBinding :: TopBinding -> Doc
pprTopBinding = \case
  StgTopLifted (StgNonRec b r)  -> pprTopBind (b,r)
  StgTopLifted (StgRec bs)      -> vsep (fmap pprTopBind bs)
  StgTopStringLit b s           -> pprTopBind' (\(Id b') str -> pprBinder b' <+> text "=" <+> (textS . show $ str)) (b,s)
  where
    pprTopBind = pprTopBind' pprRhs
    pprTopBind' f (b, rhs) = sep
      [ pprBinderTypeSig b
      , f (Id b) rhs
      , mempty
      ]
instance Pretty TopBinding where
  pretty :: TopBinding -> Doc
  pretty = pprTopBinding

ppTyConName :: TyCon -> Doc
ppTyConName TyCon{..} = {-pretty tcUnitId <> text "_" <> pretty tcModule <> text "." <> -}pretty tcName

pprTyCon :: TyCon -> Doc
pprTyCon TyCon{..} = {-pretty tcUnitId <> text "_" <> pretty tcModule <> text "." <> -}
  text "data" <+> pretty tcName <$$> indent 2 (vsep ([text c <+> pprDataConDef dc | (dc, c) <- zip tcDataCons ("=" : repeat "|")]))

pprDataConDef :: DataCon -> Doc
pprDataConDef DataCon{..} = case dcRep of
  AlgDataCon dcArgsRep -> pretty dcName <+> hsep (fmap ppPrimRep dcArgsRep)
  x                    -> textS $ "-- " ++ show x

pprDataConName :: DataCon -> Doc
pprDataConName DataCon{..} = {-pretty dcUnitId <> text "_" <> pretty dcModule <> text "." <> -}pretty dcName{- <+> text "::" <+> textS (show dcRep) <+> parens (textS (show dcId))-}

{-
instance Pretty DataCon where
    pretty = pprDataCon
-}
pprModule :: Module -> Doc
pprModule Module{..} = vsep
  [ text "-- package:" <+> pretty moduleUnitId
  , text "-- source-file-path:" <+> pretty (fromMaybe "<empty>" moduleSourceFilePath)
  , text "module" <+> pretty moduleName
  , indent 2 $ pprExportList moduleTopBindings
  , "  ) where"
  , mempty
  , vsep [pprImportList u mod il | (u, ml) <- moduleExternalTopIds, (mod, il) <- ml]
  , mempty
  , vsep
    [ pprTyCon tc <> newline
    | (uid, ml) <- moduleTyCons
    , uid == moduleUnitId
    , (modName, tl) <- ml
    , modName == moduleName
    , tc <- tl
    ]
  , vsep (fmap pprTopBinding moduleTopBindings)
  ]

pprImportList :: UnitId -> ModuleName -> [Binder] -> Doc
pprImportList u mod bl = text "import" <+> text "\"" <> pretty u <> text "\"" <+> pretty mod <+> align (collection "(" ")" "," $ fmap pprVar bl)

pprExportList :: [TopBinding] -> Doc
pprExportList l = vsep
  [ text t <+> pprVar b
  | (b, t) <- zip exportedBinders ("(" : repeat ",")
  ]
  where
    exportedBinders = filter ((ModulePublic ==) . binderScope) $ getTopBinders l

getTopBinders :: [TopBinding] -> [Binder]
getTopBinders = concatMap go
  where
    go = \case
      StgTopStringLit b _ -> [b]
      StgTopLifted (StgNonRec b _) -> [b]
      StgTopLifted (StgRec l) -> fmap fst l

instance Pretty Module where
  pretty :: Module -> Doc
  pretty = pprModule

pprForeignStubs :: ForeignStubs -> Doc
pprForeignStubs = \case
  NoStubs           -> empty
  ForeignStubs{..}  -> vsep
                        [ text "foreign stub C header {" <$$> green (pretty fsCHeader) <$$> text "}"
                        , text "foreign stub C source {" <$$> green (pretty fsCSource) <$$> text "}"
                        , text "foreign decls {" <$$> indent 2 (vsep $ fmap (textS . show) fsDecls) <$$> text "}"
                        ]

comment :: Doc -> Doc
comment x = text "{-" <+> x <+> text "-}"

-------------------
-- rendering

data StgPointState
  = StgPointState
  { spsRow       :: Int
  , spsCol       :: Int
  , spsStgPoints :: [(StgPoint, SrcRange)]
  , spsOutput    :: [Text]
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
getPos = gets ((,) . spsRow) <*> gets spsCol

pShow :: Doc -> (Text, [(StgPoint, SrcRange)])
pShow = pShowWithConfig Config {cfgPrintTickish = False}

pShowWithConfig :: Config -> Doc -> (Text, [(StgPoint, SrcRange)])
pShowWithConfig cfg doc = (T.concat . reverse $ spsOutput result, reverse $ spsStgPoints result)
  where
    result = execState (renderPOut $ execDoc cfg doc) emptyStgPointState

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
