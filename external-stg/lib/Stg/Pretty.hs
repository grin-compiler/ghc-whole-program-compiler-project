{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stg.Pretty
  ( Pretty(..)
  , module Stg.Pretty
  , plain
  ) where

import Stg.Syntax

import Data.Ratio
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen

{-
data ForeignCall
data PrimCall = PrimCall -- Name Name
data UpdateFlag = ReEntrant | Updatable | SingleEntry
-}


smallRArrow :: Doc
smallRArrow = "->"

hang' :: Doc -> Int -> Doc -> Doc
hang' d1 n d2 = hang n $ sep [d1, d2]

comment :: Doc -> Doc
comment x = "{-" <+> x <+> "-}"

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
  VecRep i r  -> angles $ pretty i <+> comma <+> text (show r)
  r -> text $ show r

colorBinderExport :: Binder -> Doc -> Doc
colorBinderExport b = case binderScope b of
  LocalScope      -> id
  GlobalScope     -> id
  HaskellExported -> green
  ForeignExported -> ondullcyan . green

pprBinder :: Binder -> Doc
pprBinder b = parens $ (colorBinderExport b . pretty . binderUniqueName $ b) <+> text ":" <+> ppType (binderType b) <+> parens (pretty $ binderTypeSig b) <+> parens (pretty $ show $ binderDetails b) where
  BinderId u  = binderId b

pprVar :: Binder -> Doc
pprVar b = colorBinderExport b . pretty . binderUniqueName $ b where
  BinderId u  = binderId b

instance Pretty Type where
    pretty = ppType

instance Pretty Name where
    pretty = text . BS.unpack

instance Pretty UnitId where
    pretty = text . BS.unpack . getUnitId

instance Pretty ModuleName where
    pretty = text . BS.unpack . getModuleName

pprRational :: Rational -> Doc
pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance Pretty LitNumType where
  pretty = \case
    LitNumInt     -> "Int"
    LitNumInt64   -> "Int64"
    LitNumWord    -> "Word"
    LitNumWord64  -> "Word64"

instance Pretty Lit where
    pretty (LitChar x) = "'" <> char x <> "'#"
    pretty (LitString x) = "\"" <> text (BS.unpack x) <> "\"#"
    pretty LitNullAddr = "nullAddr#"
    pretty (LitFloat x) = "FLOAT" <> parens (pprRational x)
    pretty (LitDouble x) = "DOUBLE" <> parens (pprRational x)
    pretty (LitLabel x s) = "LABEL"<> parens (pretty x) <+> text (show s)
    pretty (LitNumber t i) = "#" <> pretty t <> "#" <> pretty i

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

pprAlt :: Alt -> Doc
pprAlt (Alt con bndrs rhs) = (hsep (pretty con : map (pprBinder) bndrs) <+> smallRArrow) <$$> indent 2 (pprExpr' False rhs)

pprArg :: Arg -> Doc
pprArg = \case
  StgVarArg o -> pprVar o
  StgLitArg l -> pretty l

instance Pretty Safety where
  pretty = text . show

instance Pretty CCallConv where
  pretty = text . show

instance Pretty CCallTarget where
  pretty = text . show

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
pprExpr' hasParens exp = case exp of
  StgLit l            -> pretty l
  StgCase x b at alts -> maybeParens hasParens
                         $ sep [ hsep [ "case" <+> pprExpr' False x, "of" <+> pprBinder b <+> text ":" <+> parens (pretty at) <+> "{" ]
                               , indent 2 $ vcat $ map (pprAlt) alts
                               , "}"
                               ]
  StgApp f args ty s    -> maybeParens hasParens $ (pprVar f) <+> (hsep $ map (pprArg) args) <+> text "::" <+> (pretty ty)-- <+> comment (pretty s)
  StgOpApp op args ty n -> maybeParens hasParens $ (pprOp op) <+> (hsep $ map (pprArg) args) <+> text "::" <+> (pretty ty) <+> maybe mempty (parens . ppTyConName) n
  StgConApp dc args _t  -> maybeParens hasParens $ (pretty dc) <+> (hsep $ map (pprArg) args)
  StgLet b e            -> maybeParens hasParens $ "let" <+> (align $ pprBinding b) <$$> "in" <+> align (pprExpr' False e)
  StgLetNoEscape b e    -> maybeParens hasParens $ "lettail" <+> (align $ pprBinding b) <$$> "in" <+> align (pprExpr' False e)
  StgTick _tickish e    -> pprExpr' hasParens e

instance Pretty Expr where
  pretty = pprExpr


pprRhs :: Rhs -> Doc
pprRhs = \case
  StgRhsClosure _ u bs e -> text "\\closure" <+> hsep (map pprBinder bs) <+> text "->" <+> braces (line <> pprExpr e)
  StgRhsCon d vs -> pretty d <+> (hsep $ map (pprArg) vs)

pprBinding :: Binding -> Doc
pprBinding = \case
  StgNonRec b r  -> pprTopBind (b,r)
  StgRec bs      -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  where
    pprTopBind (b,rhs) =
      (pprBinder b <+> equals <$$> (indent 2 $ pprRhs rhs))
      <> line

pprTopBinding :: TopBinding -> Doc
pprTopBinding = \case
  StgTopLifted (StgNonRec b r)  -> pprTopBind (b,r)
  StgTopLifted (StgRec bs)      -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  StgTopStringLit b s           -> pprTopBind' (text . show) (b,s)
  where
    pprTopBind = pprTopBind' pprRhs
    pprTopBind' f (b,rhs) =
      (pprBinder b <+> equals <$$> (indent 2 $ f rhs))
      <> line

instance Pretty TopBinding where
  pretty = pprTopBinding

ppTyConName :: TyCon -> Doc
ppTyConName TyCon{..} = pretty tcUnitId <> text "_" <> pretty tcModule <> text "." <> pretty tcName

pprTyCon :: TyCon -> Doc
pprTyCon TyCon{..} = pretty tcUnitId <> text "_" <> pretty tcModule <> text "." <> pretty tcName <$$> (indent 2 $ vsep (map pretty tcDataCons)) <> line where

pprDataCon :: DataCon -> Doc
pprDataCon DataCon{..} = pretty dcUnitId <> text "_" <> pretty dcModule <> text "." <> pretty dcName <+> text "::" <+> text (show dcRep) <+> parens (text (show dcId))

instance Pretty DataCon where
    pretty = pprDataCon
{-
instance Pretty TyCon where
  pretty = pprTyCon
-}
pprModule :: Module -> Doc
pprModule m =
  comment (pretty $ modulePhase m)
  <$$> text "package" <+> pretty (moduleUnitId m)
  <$$> text "module" <+> pretty (moduleName m) <+> "where" <> line

  <$$> vsep [text "using" <+> pretty u <+> text ":" <+> pretty mod | (u, ml) <- moduleDependency m, mod <- ml] <> line

  <$$> text "externals" <$$> vsep [indent 2 $ vsep (map pprBinder bl) | (_, ml) <- moduleExternalTopIds m, (_, bl) <- ml] <> line

  <$$> text "type" <$$> vsep [indent 2 $ vsep (map pprTyCon tl) | (_, ml) <- moduleTyCons m, (_, tl) <- ml] <> line

  <$$> vsep (map (pprTopBinding) (moduleTopBindings m))

  <$$> pprForeignStubs (moduleForeignStubs m)
  <$$> pprForeignFiles (moduleForeignFiles m)

instance Pretty Module where
  pretty = pprModule

pprForeignStubs :: ForeignStubs -> Doc
pprForeignStubs = \case
  NoStubs           -> empty
  ForeignStubs{..}  -> vsep
                        [ text "foreign stub C header {" <$$> green (pretty fsCHeader) <$$> text "}"
                        , text "foreign stub C source {" <$$> green (pretty fsCSource) <$$> text "}"
                        ]

pprForeignFiles :: [(ForeignSrcLang, FilePath)] -> Doc
pprForeignFiles l = text "foreign files" <$$> vsep [indent 2 (text (show t) <+> text p) | (t, p) <- l]
