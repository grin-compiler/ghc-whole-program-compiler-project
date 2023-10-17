{-# LANGUAGE RecordWildCards #-}
module Stg.IRLocation where

import Stg.Syntax

data StgId
  = StgId
  { siUnitId      :: Name
  , siModuleName  :: Name
  , siName        :: Name
  , siUnique      :: Maybe Unique
  }
  deriving (Eq, Ord, Show, Read)

binderToStgId :: Binder -> StgId
binderToStgId Binder{..} = StgId
  { siUnitId      = getUnitId binderUnitId
  , siModuleName  = getModuleName binderModule
  , siName        = binderName
  , siUnique      = case binderScope of
                      ModulePublic -> Nothing
                      _ | BinderId u <- binderId
                        -> Just u
  }

data StgPoint
  -- expression locations
  = SP_CaseScrutineeExpr  { spScrutineeResultName :: StgId }
  | SP_LetExpr            { spParent :: StgPoint }
  | SP_LetNoEscapeExpr    { spParent :: StgPoint }
  | SP_RhsClosureExpr     { spRhsBinderName :: StgId }
  | SP_AltExpr            { spScrutineeResultName :: StgId, spAltIndex :: Int }
  | SP_RhsCon             { spRhsBinderName :: StgId }
  | SP_Binding            { spBinderName :: StgId }
  | SP_Tickish            { spParent :: StgPoint }
  deriving (Eq, Ord, Show, Read)

{-
  breakpoint types:
    - program point: IRPath
    - primop name
    - foreign symbol name
-}

{-
  unitid        - name
  Module        - name
   topbinding   - name
    binding     - name
     rhs
      arg
      expr
       arg
       expr
       binding
       alt
        expr
-}
{-
  identification methods:
    - unique name
    - index (this means the parent's children index, e.g. Case's Alt chidren)
-}
{-
  StgPoint location root:
    - top IR data structure: [Module]
    - uniqueNamed
        TopBinding
        Binding (StgNonRec binding)
        Rhs (StgRec binding)
        Expr (StgCase)
-}
{-
  IR elements with unique names:
    - StgCase
    - StgNonRec
    - any Rhs ; from StgRec
    - StgTopStringLit
-}

-- can select any type that StgIR can represent
data FieldSelector
  -- generic
  = FS_UniqueName                 Name  -- selects uniquely named IR element
  | FS_Binder                     StgId -- selects uniquely named IR element

  -- Module
  | FS_Module_moduleTopBindings   Int   -- selects: Module     -> TopBinding
  -- TopBinding
  | FS_StgTopLifted_arg0                -- selects: TopBinding -> Binding
  -- Binding
  | FS_StgNonRec_arg0                   -- selects: Binding    -> Rhs
  | FS_StgRec_arg0                Int   -- selects: Binding    -> Rhs
  -- Rhs
  | FS_StgRhsClosure_arg0         Int   -- selects: Rhs        -> Binder
  | FS_StgRhsClosure_arg2         Int   -- selects: Rhs        -> Binder
  | FS_StgRhsClosure_arg3               -- selects: Rhs        -> Expr
  | FS_StgRhsCon_arg1             Int   -- selects: Rhs        -> Arg
  -- Arg
  | FS_StgVarArg_arg0                   -- selects: Arg        -> Binder
  -- Expr
  | FS_StgApp_arg0                      -- selects: Expr       -> Binder
  | FS_StgApp_arg1                Int   -- selects: Expr       -> Arg
  | FS_StgConApp_arg1             Int   -- selects: Expr       -> Arg
  | FS_StgOpApp_arg1              Int   -- selects: Expr       -> Arg
  | FS_StgCase_arg0                     -- selects: Expr       -> Expr
  | FS_StgCase_arg1                     -- selects: Expr       -> Binder
  | FS_StgCase_arg3               Int   -- selects: Expr       -> Alt
  | FS_StgLet_arg0                      -- selects: Expr       -> Binding
  | FS_StgLet_arg1                      -- selects: Expr       -> Expr
  | FS_StgLetNoEscape_arg0              -- selects: Expr       -> Binding
  | FS_StgLetNoEscape_arg1              -- selects: Expr       -> Expr
  | FS_StgTick_arg0                     -- selects: Expr       -> Expr
  -- Alt
  | FS_Alt_altBinders             Int   -- selects: Alt        -> Binder
  | FS_Alt_altRHS                       -- selects: Alt        -> Expr
  deriving (Eq, Ord, Show)

type IRPath = [FieldSelector]

data IR
  = IR_Module     Module
  | IR_TopBinding TopBinding
  | IR_Binding    Binding
  | IR_Rhs        Rhs
  | IR_Arg        Arg
  | IR_Expr       Expr
  | IR_Alt        Alt
  | IR_Binder     Binder
  deriving (Eq, Ord, Show)

lookupIR :: IR -> IRPath -> IR
lookupIR = undefined
