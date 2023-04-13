module Stg.JSON where

import Data.Aeson
import Stg.Syntax

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Lazy as Text

instance ToJSON BS8.ByteString where
  toJSON = toJSON . Text.pack . BS8.unpack

instance ToJSON Unique
instance ToJSON RealSrcSpan
instance ToJSON BufSpan
instance ToJSON UnhelpfulSpanReason
instance ToJSON SrcSpan
instance ToJSON Tickish
instance ToJSON PrimRep
instance ToJSON PrimElemRep
instance ToJSON Type
instance ToJSON STyCon
instance ToJSON TyConId
instance ToJSON DataConId
instance ToJSON DataConRep
instance ToJSON SDataCon
instance ToJSON CbvMark
instance ToJSON IdDetails
instance ToJSON UnitId
instance ToJSON ModuleName
instance ToJSON BinderId
instance ToJSON SBinder
instance ToJSON Scope
instance ToJSON LitNumType
instance ToJSON LabelSpec
instance ToJSON Lit
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (TopBinding' a b c d)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (Binding' a b c d)
instance (ToJSON a) => ToJSON (Arg' a)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (Expr' a b c d)
instance (ToJSON a) => ToJSON (AltType' a)
instance ToJSON UpdateFlag
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (Rhs' a b c d)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (Alt' a b c d)
instance (ToJSON a) => ToJSON (AltCon' a)
instance ToJSON Safety
instance ToJSON CCallConv
instance ToJSON SourceText
instance ToJSON CCallTarget
instance ToJSON ForeignCall
instance ToJSON PrimCall
instance ToJSON StgOp
instance ToJSON Header
instance ToJSON CImportSpec
instance ToJSON CExportSpec
instance ToJSON ForeignImport
instance ToJSON ForeignExport
instance ToJSON StubImpl
instance ToJSON ModuleLabelKind
instance ToJSON ModuleCLabel
instance (ToJSON idOcc) => ToJSON (StubDecl' idOcc)
instance (ToJSON idOcc) => ToJSON (ForeignStubs' idOcc)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (Module' a b c d e)

instance FromJSON BS8.ByteString where
  parseJSON = fmap (BS8.pack . Text.unpack) . parseJSON

instance FromJSON Unique
instance FromJSON RealSrcSpan
instance FromJSON BufSpan
instance FromJSON UnhelpfulSpanReason
instance FromJSON SrcSpan
instance FromJSON Tickish
instance FromJSON PrimRep
instance FromJSON PrimElemRep
instance FromJSON Type
instance FromJSON STyCon
instance FromJSON TyConId
instance FromJSON DataConId
instance FromJSON DataConRep
instance FromJSON SDataCon
instance FromJSON CbvMark
instance FromJSON IdDetails
instance FromJSON UnitId
instance FromJSON ModuleName
instance FromJSON BinderId
instance FromJSON SBinder
instance FromJSON Binder
instance FromJSON Scope
instance FromJSON LitNumType
instance FromJSON LabelSpec
instance FromJSON Lit
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (TopBinding' a b c d)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (Binding' a b c d)
instance (FromJSON a) => FromJSON (Arg' a)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (Expr' a b c d)
instance (FromJSON a) => FromJSON (AltType' a)
instance FromJSON UpdateFlag
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (Rhs' a b c d)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (Alt' a b c d)
instance FromJSON a => FromJSON (AltCon' a)
instance FromJSON Safety
instance FromJSON CCallConv
instance FromJSON SourceText
instance FromJSON CCallTarget
instance FromJSON ForeignCall
instance FromJSON PrimCall
instance FromJSON StgOp
instance FromJSON Header
instance FromJSON CImportSpec
instance FromJSON CExportSpec
instance FromJSON ForeignImport
instance FromJSON ForeignExport
instance FromJSON StubImpl
instance FromJSON ModuleLabelKind
instance FromJSON ModuleCLabel
instance (FromJSON idOcc) => FromJSON (StubDecl' idOcc)
instance (FromJSON idOcc) => FromJSON (ForeignStubs' idOcc)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON (Module' a b c d e)
