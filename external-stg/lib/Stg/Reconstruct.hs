{-# OPTIONS_GHC -Wno-orphans #-}

module Stg.Reconstruct (reconModule, topBindings) where

import           Control.Applicative ((<$>))
import           Control.Monad       (Functor (..))

import           Data.Bool           (Bool (..))
import           Data.Foldable       (Foldable (..), concatMap)
import           Data.Function       (flip, ($), (.))
import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Lazy   as HM
import           Data.Int            (Int)
import           Data.List           (unlines, zip, (++))
import           Data.Maybe          (Maybe (..))
import           Data.Tuple          (fst, snd)

import           GHC.Err             (error)

import           Stg.Syntax          (Alt, Alt' (..), AltCon, AltCon' (..), AltType, AltType' (..), Arg, Arg' (..),
                                      Binder (..), BinderId (..), Binding, Binding' (..), CutTyCon (..), DataCon (..),
                                      DataConId (..), Expr, Expr' (..), ForeignStubs, ForeignStubs' (..), Module,
                                      Module' (..), ModuleName, Rhs, Rhs' (..), SAlt, SAltCon, SAltType, SArg,
                                      SBinder (..), SBinding, SDataCon (..), SExpr, SForeignStubs, SModule, SRhs,
                                      SStubDecl, STopBinding, STyCon (..), Scope (..), StubDecl, StubDecl' (..),
                                      TopBinding, TopBinding' (..), TyCon (..), TyConId (..), Unique (..), UnitId,
                                      mkBinderUniqueName, mkDataConUniqueName, mkTyConUniqueName)

import           Text.Show           (Show (..))

instance Hashable BinderId where
    hashWithSalt :: Int -> BinderId -> Int
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

instance Hashable DataConId where
    hashWithSalt :: Int -> DataConId -> Int
    hashWithSalt salt (DataConId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

instance Hashable TyConId where
    hashWithSalt :: Int -> TyConId -> Int
    hashWithSalt salt (TyConId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

data BinderMap
  = BinderMap
  { bmUnitId     :: UnitId
  , bmModule     :: ModuleName
  , bmIdMap      :: HM.HashMap BinderId Binder
  , bmDataConMap :: HM.HashMap DataConId DataCon
  , bmTyConMap   :: HM.HashMap TyConId TyCon
  }

-- Id handling
insertBinder :: Binder -> BinderMap -> BinderMap
insertBinder b bm@BinderMap{..} = bm {bmIdMap = HM.insert (binderId b) b bmIdMap}

insertBinders :: [Binder] -> BinderMap -> BinderMap
insertBinders bs bm = foldl' (flip insertBinder) bm bs

getBinder :: BinderMap -> BinderId -> Binder
getBinder BinderMap{..} bid = case HM.lookup bid bmIdMap of
  Just b  -> b
  Nothing -> error $ "unknown binder " ++ show bid ++ ":\nin scope:\n" ++
              unlines (fmap (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList bmIdMap))

{-
-- DataCon handling
insertDataCon :: DataCon -> BinderMap -> BinderMap
insertDataCon dc bm@BinderMap{..} = bm {bmDataConMap = HM.insert (dcId dc) dc bmDataConMap}

insertDataCons :: [DataCon] -> BinderMap -> BinderMap
insertDataCons dcs bm = foldl' (flip insertDataCon) bm dcs
-}

getDataCon :: BinderMap -> DataConId -> DataCon
getDataCon BinderMap{..} bid = case HM.lookup bid bmDataConMap of
  Just b  -> b
  Nothing -> error $ "unknown data con "++ show bid ++ ":\nin scope:\n" ++
              unlines (fmap (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList bmDataConMap))

-- TyCon handling
getTyCon :: BinderMap -> TyConId -> TyCon
getTyCon BinderMap{..} i = case HM.lookup i bmTyConMap of
  Just b  -> b
  Nothing -> error $ "unknown ty con "++ show i ++ ":\nin scope:\n" ++
              unlines (fmap (\(i',b) -> show i' ++ "\t" ++ show b) (HM.toList bmTyConMap))



-- "recon" == "reconstruct"

reconLocalBinder :: BinderMap -> SBinder -> Binder
reconLocalBinder BinderMap{..} b@SBinder{..} = -- HINT: local binders only
  Binder
  { binderName        = sbinderName
  , binderId          = sbinderId
  , binderType        = sbinderType
  , binderTypeSig     = sbinderTypeSig
  , binderDetails     = sbinderDetails
  , binderInfo        = sbinderInfo
  , binderDefLoc      = sbinderDefLoc
  , binderUnitId      = bmUnitId
  , binderModule      = bmModule
  , binderScope       = ClosurePrivate
  , binderTopLevel    = False
  , binderUniqueName  = uName
  , binderUNameHash   = hash uName
  } where uName = mkBinderUniqueName False bmUnitId bmModule b

reconDataCon :: UnitId -> ModuleName -> TyCon -> SDataCon -> DataCon
reconDataCon u m tc sdc@SDataCon{..} = DataCon
  { dcName    = sdcName
  , dcId      = sdcId
  , dcUnitId  = u
  , dcModule  = m
  , dcRep     = sdcRep
  , dcWorker  = mkTopBinder u m (sbinderScope sdcWorker) sdcWorker
  , dcDefLoc  = sdcDefLoc
  , dcTyCon   = CutTyCon tc
  , dcUniqueName  = uName
  , dcUNameHash   = hash uName
  } where uName = mkDataConUniqueName u m sdc

reconTyCon :: UnitId -> ModuleName -> STyCon -> TyCon
reconTyCon u m stc@STyCon{..} = tc where
  tc = TyCon
    { tcName        = stcName
    , tcId          = stcId
    , tcUnitId      = u
    , tcModule      = m
    , tcDataCons    = fmap (reconDataCon u m tc) stcDataCons
    , tcDefLoc      = stcDefLoc
    , tcUniqueName  = uName
    , tcUNameHash   = hash uName
    } where uName = mkTyConUniqueName u m stc

mkTopBinder :: UnitId -> ModuleName -> Scope -> SBinder -> Binder
mkTopBinder u m scope b@SBinder{..} =
  Binder
  { binderName        = sbinderName
  , binderId          = sbinderId
  , binderType        = sbinderType
  , binderTypeSig     = sbinderTypeSig
  , binderDetails     = sbinderDetails
  , binderInfo        = sbinderInfo
  , binderDefLoc      = sbinderDefLoc
  , binderUnitId      = u
  , binderModule      = m
  , binderScope       = scope
  , binderTopLevel    = True
  , binderUniqueName  = uName
  , binderUNameHash   = hash uName
  } where uName = mkBinderUniqueName True u m (b {sbinderScope = scope})

reconModule :: SModule -> Module
reconModule Module{..} = mod where
  mod = Module
    { modulePhase               = modulePhase
    , moduleUnitId              = moduleUnitId
    , moduleName                = moduleName
    , moduleSourceFilePath      = moduleSourceFilePath
    , moduleForeignStubs        = stubs
    , moduleHasForeignExported  = moduleHasForeignExported
    , moduleDependency          = moduleDependency
    , moduleExternalTopIds      = exts
    , moduleTyCons              = tyConList
    , moduleTopBindings         = binds
    }

  bm = BinderMap
       { bmUnitId      = moduleUnitId
       , bmModule      = moduleName
       , bmIdMap       = HM.fromList [(binderId b, b) | b <- tops ++ concatMap snd (concatMap snd exts)]
       , bmDataConMap  = HM.fromList [(dcId dc, dc) | dc <- cons]
       , bmTyConMap    = HM.fromList [(tcId tc, tc) | tc <- tyCons]
       }

  tyCons :: [TyCon]
  tyCons = concatMap (concatMap snd . snd) tyConList

  cons :: [DataCon]
  cons = concatMap tcDataCons tyCons

  tyConList :: [(UnitId, [(ModuleName, [TyCon])])]
  tyConList = [(u, [(m, fmap (reconTyCon u m) l) | (m, l) <- ml]) | (u, ml) <- moduleTyCons]

  stubs :: ForeignStubs
  stubs = reconForeignStubs bm moduleForeignStubs

  binds :: [TopBinding]
  binds = fmap reconTopBinding moduleTopBindings

  tops :: [Binder]
  tops  = [ mkTopBinder moduleUnitId moduleName sbinderScope b
          | b@SBinder{..} <- concatMap topBindings moduleTopBindings
          ]

  exts :: [(UnitId, [(ModuleName, [Binder])])]
  exts = [(u, [(m, fmap (mkTopBinder u m ModulePublic) l) | (m, l) <- ml]) | (u, ml) <- moduleExternalTopIds]

  reconTopBinder :: SBinder -> Binder
  reconTopBinder b = getBinder bm $ sbinderId b

  reconTopBinding :: STopBinding -> TopBinding
  reconTopBinding = \case
    StgTopStringLit b s           -> StgTopStringLit (reconTopBinder b) s
    StgTopLifted (StgNonRec b r)  -> StgTopLifted $ StgNonRec (reconTopBinder b) (reconRhs bm r)
    StgTopLifted (StgRec bs)      -> StgTopLifted $ StgRec [(reconTopBinder b, reconRhs bm r) | (b,r) <- bs]

reconForeignStubs :: BinderMap -> SForeignStubs -> ForeignStubs
reconForeignStubs bm = \case
  NoStubs                 -> NoStubs
  ForeignStubs h c i f l  -> ForeignStubs h c i f $ fmap (reconStubDecl bm) l

reconStubDecl :: BinderMap -> SStubDecl -> StubDecl
reconStubDecl bm = \case
  StubDeclImport f m    -> StubDeclImport f m
  StubDeclExport f i s  -> StubDeclExport f (getBinder bm i) s

topBindings :: TopBinding' idBnd idOcc dcOcc tcOcc -> [idBnd]
topBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> fmap fst bs
  StgTopStringLit b _           -> [b]

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l              -> StgLit l
  StgCase x b at alts   -> let b'   = reconLocalBinder bm b
                               bm'  = insertBinder b' bm
                           in StgCase (reconExpr bm x) b' (reconAltType bm at) (fmap (reconAlt bm') alts)
  StgApp f args         -> StgApp (getBinder bm f) (fmap (reconArg bm) args)
  StgOpApp op args t tc -> StgOpApp op (fmap (reconArg bm) args) t (getTyCon bm <$> tc)
  StgConApp dc args t   -> StgConApp (getDataCon bm dc) (fmap (reconArg bm) args) t
  StgLet b e            -> let (bm', b') = reconBinding bm b
                           in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e    -> let (bm', b') = reconBinding bm b
                           in StgLetNoEscape b' (reconExpr bm' e)
  StgTick t e           -> StgTick t (reconExpr bm e)

reconBinding :: BinderMap -> SBinding -> (BinderMap, Binding)
reconBinding bm = \case
  StgNonRec b r -> let b'   = reconLocalBinder bm b
                       bm'  = insertBinder b' bm
                   in (bm', StgNonRec b' (reconRhs bm' r))
  StgRec bs     -> let bs'  = fmap (reconLocalBinder bm . fst) bs
                       bm'  = insertBinders bs' bm
                   in (bm', StgRec [(b, reconRhs bm' r) | ((_,r), b) <- zip bs bs'])

reconRhs :: BinderMap -> SRhs -> Rhs
reconRhs bm = \case
  StgRhsCon dc vs         -> StgRhsCon (getDataCon bm dc) $ fmap (reconArg bm) vs
  StgRhsClosure fs u bs e -> let  fs' = fmap (getBinder bm) fs
                                  bs' = fmap (reconLocalBinder bm) bs
                                  bm' = insertBinders bs' bm
                              in StgRhsClosure fs' u bs' (reconExpr bm' e)

reconArg :: BinderMap -> SArg -> Arg
reconArg bm = \case
  StgVarArg b -> StgVarArg $ getBinder bm b
  StgLitArg l -> StgLitArg l

reconAlt :: BinderMap -> SAlt -> Alt
reconAlt bm (Alt con bs rhs) =
    let bs' = fmap (reconLocalBinder bm) bs
        bm' = insertBinders bs' bm
    in Alt (reconAltCon bm con) bs' (reconExpr bm' rhs)

reconAltCon :: BinderMap -> SAltCon -> AltCon
reconAltCon bm = \case
  AltDataCon dc -> AltDataCon $ getDataCon bm dc
  AltLit l      -> AltLit l
  AltDefault    -> AltDefault

reconAltType :: BinderMap -> SAltType -> AltType
reconAltType bm = \case
  PolyAlt       -> PolyAlt
  MultiValAlt i -> MultiValAlt i
  PrimAlt r     -> PrimAlt r
  AlgAlt tc     -> AlgAlt $ getTyCon bm tc
