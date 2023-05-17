module Stg.IRLocation where

import Stg.Syntax

data StgPoint
  -- expression locations
  = SP_CaseScrutineeExpr  { spScrutineeResultName :: Name }
  | SP_LetExpr            { spParent :: StgPoint }
  | SP_LetNoEscapeExpr    { spParent :: StgPoint }
  | SP_RhsClosureExpr     { spRhsBinderName :: Name }
  | SP_AltExpr            { spScrutineeResultName :: Name, spAltIndex :: Int }
  | SP_RhsCon             { spRhsBinderName :: Name }
  deriving (Eq, Ord, Show)
