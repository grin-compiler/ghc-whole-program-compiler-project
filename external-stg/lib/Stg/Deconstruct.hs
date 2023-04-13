{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Deconstruct (deconModule) where

import Stg.Syntax

deconModule :: Module -> SModule
deconModule Module{..} = smod where
  smod = Module
    { modulePhase               = modulePhase
    , moduleUnitId              = moduleUnitId
    , moduleName                = moduleName
    , moduleSourceFilePath      = moduleSourceFilePath
    , moduleForeignStubs        = deconForeignStubs moduleForeignStubs
    , moduleHasForeignExported  = moduleHasForeignExported
    , moduleDependency          = moduleDependency
    , moduleExternalTopIds      = [(uid, [(m, map deconIdBnd ids) | (m, ids) <- mods]) | (uid, mods) <- moduleExternalTopIds]
    , moduleTyCons              = [(uid, [(m, map deconTcBnd tcs) | (m, tcs) <- mods]) | (uid, mods) <- moduleTyCons]
    , moduleTopBindings         = map deconTopBinding moduleTopBindings
    }

deconIdBnd :: Binder -> SBinder
deconIdBnd Binder{..} =
  SBinder
    { sbinderName     = binderName
    , sbinderId       = binderId
    , sbinderType     = binderType
    , sbinderTypeSig  = binderTypeSig
    , sbinderScope    = binderScope
    , sbinderDetails  = binderDetails
    , sbinderInfo     = binderInfo
    , sbinderDefLoc   = binderDefLoc
    }

deconTcBnd :: TyCon -> STyCon
deconTcBnd TyCon{..} =
  STyCon
    { stcName     = tcName
    , stcId       = tcId
    , stcDataCons = map deconDcBnd tcDataCons
    , stcDefLoc   = tcDefLoc
    }

deconDcBnd :: DataCon -> SDataCon
deconDcBnd DataCon{..} =
  SDataCon
    { sdcName   = dcName
    , sdcId     = dcId
    , sdcRep    = dcRep
    , sdcWorker = deconIdBnd dcWorker
    , sdcDefLoc = dcDefLoc
    }

deconIdOcc :: Binder -> BinderId
deconIdOcc b = binderId b

deconTcOcc :: TyCon -> TyConId
deconTcOcc tc = tcId tc

deconDcOcc :: DataCon -> DataConId
deconDcOcc dc = dcId dc

deconTopBinding :: TopBinding -> STopBinding
deconTopBinding = \case
  StgTopLifted binding  -> StgTopLifted $ deconBinding binding
  StgTopStringLit b str -> StgTopStringLit (deconIdBnd b) str

deconBinding :: Binding -> SBinding
deconBinding = \case
  StgNonRec b rhs -> StgNonRec (deconIdBnd b) (deconRhs rhs)
  StgRec rhsList  -> StgRec [(deconIdBnd b, deconRhs rhs) | (b, rhs) <- rhsList]

deconRhs :: Rhs -> SRhs
deconRhs = \case
  StgRhsClosure idOccList uf idBndList e  -> StgRhsClosure (map deconIdOcc idOccList) uf (map deconIdBnd idBndList) (deconExpr e)
  StgRhsCon dcOcc args                    -> StgRhsCon (deconDcOcc dcOcc) (map deconArg args)

deconArg :: Arg -> SArg
deconArg = \case
  StgVarArg idOcc -> StgVarArg (deconIdOcc idOcc)
  StgLitArg lit   -> StgLitArg lit

deconExpr :: Expr -> SExpr
deconExpr = \case
  StgApp idOcc args       -> StgApp (deconIdOcc idOcc) (map deconArg args)
  StgLit lit              -> StgLit lit
  StgConApp dcOcc args ts -> StgConApp (deconDcOcc dcOcc) (map deconArg args) ts
  StgOpApp op args t mTc  -> StgOpApp op (map deconArg args) t (fmap deconTcOcc mTc)
  StgCase e idBnd at alts -> StgCase (deconExpr e) (deconIdBnd idBnd) (deconAltType at) (map deconAlt alts)
  StgLet b e              -> StgLet (deconBinding b) (deconExpr e)
  StgLetNoEscape b e      -> StgLetNoEscape (deconBinding b) (deconExpr e)
  StgTick t e             -> StgTick t (deconExpr e)

deconAltType :: AltType -> SAltType
deconAltType = \case
  PolyAlt       -> PolyAlt
  MultiValAlt i -> MultiValAlt i
  PrimAlt r     -> PrimAlt r
  AlgAlt tcOcc  -> AlgAlt (deconTcOcc tcOcc)

deconAlt :: Alt -> SAlt
deconAlt Alt{..} = Alt (deconAltCon altCon) (map deconIdBnd altBinders) (deconExpr altRHS)

deconAltCon :: AltCon -> SAltCon
deconAltCon = \case
  AltDataCon dcOcc  -> AltDataCon (deconDcOcc dcOcc)
  AltLit lit        -> AltLit lit
  AltDefault        -> AltDefault

deconForeignStubs :: ForeignStubs -> SForeignStubs
deconForeignStubs = \case
  NoStubs                 -> NoStubs
  ForeignStubs h c i f l  -> ForeignStubs h c i f $ map deconStubDecl l

deconStubDecl :: StubDecl -> SStubDecl
deconStubDecl = \case
  StubDeclImport f m    -> StubDeclImport f m
  StubDeclExport f i s  -> StubDeclExport f (deconIdOcc i) s
