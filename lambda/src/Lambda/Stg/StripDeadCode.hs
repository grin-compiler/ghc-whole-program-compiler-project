{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, BangPatterns #-}
module Lambda.Stg.StripDeadCode (stripDeadCode, StripStat(..), LivenessFacts(..)) where

import Control.Monad.State

import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Data.ByteString.Char8 as BS8

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Hashable

import Stg.Syntax

data Env
  = Env
  { dummyLifted     :: Binder
  , deletedDefs     :: Set Name
  , deletedRefs     :: Set Name
  , deletedData     :: Set Name
  , deletedAlts     :: Set Name
  , deletedCons     :: Set Name
  , deletedTyCons   :: Set Name
  , referredIds     :: !(Set Id)
  , referredCons    :: !(Set DC)
  , referredTyCons  :: !(Set TC)
  , definedIds      :: !(Set Id)
  , dataConMap      :: Map DataConId DataCon
  }

emptyEnv :: Env
emptyEnv = Env
  { dummyLifted     = dummyBinder
  , deletedDefs     = Set.empty
  , deletedRefs     = Set.empty
  , deletedData     = Set.empty
  , deletedAlts     = Set.empty
  , deletedCons     = Set.empty
  , deletedTyCons   = Set.empty
  , referredIds     = Set.empty
  , referredCons    = Set.empty
  , referredTyCons  = Set.empty
  , definedIds      = Set.empty
  , dataConMap      = mempty
  }

type SM a = StateT Env IO a

addDefId :: Binder -> SM ()
addDefId b@Binder{..} = do
  let i = Id b
  modify' $ \env@Env{..} -> env {definedIds = if Set.member i definedIds then definedIds else Set.insert i definedIds}
  case binderDetails of
    DataConWorkId di -> addRefConId di
    DataConWrapId di -> addRefConId di
    _ -> pure ()

addRefId :: Binder -> SM ()
addRefId b = do
  let i = Id b
  modify' $ \env@Env{..} -> env {referredIds = if Set.member i referredIds then referredIds else Set.insert i referredIds}

addRefConId :: DataConId -> SM ()
addRefConId di = do
  dcMap <- gets dataConMap
  case Map.lookup di dcMap of
    Nothing -> fail $ "missing DataCon for: " ++ show di
    Just dc -> addRefCon dc

addRefCon :: DataCon -> SM ()
addRefCon dc = do
  let i = DC dc
  modify' $ \env@Env{..} -> env {referredCons = if Set.member i referredCons then referredCons else Set.insert i referredCons}

addRefTyCon :: TyCon -> SM ()
addRefTyCon tc = do
  let i = TC tc
  modify' $ \env@Env{..} -> env {referredTyCons = if Set.member i referredTyCons then referredTyCons else Set.insert i referredTyCons}

markDeletedDef :: Name -> SM ()
markDeletedDef n = do
  let msg = "deleted def: " ++ BS8.unpack n
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline
  modify' $ \env@Env{..} -> env {deletedDefs = Set.insert n deletedDefs}

markDeletedRef :: Name -> SM ()
markDeletedRef n = do
  let msg = "deleted ref: " ++ BS8.unpack n
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline
  modify' $ \env@Env{..} -> env {deletedRefs = Set.insert n deletedRefs}

markDeletedData :: Name -> SM ()
markDeletedData n = do
  let msg = "deleted data: " ++ BS8.unpack n
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline
  modify' $ \env@Env{..} -> env {deletedData = Set.insert n deletedData}

markDeletedAlt :: Name -> SM ()
markDeletedAlt n = do
  let msg = "deleted alt: " ++ BS8.unpack n
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline
  modify' $ \env@Env{..} -> env {deletedAlts = Set.insert n deletedAlts}

markDeletedCon :: DataCon -> Name -> SM ()
markDeletedCon dc n = do
  let msg = "deleted con: " ++ BS8.unpack n
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline
  modify' $ \env@Env{..} -> env {deletedCons = Set.insert n deletedCons}
  -- check if a referred is deleted
  Env{..} <- get
  when (Set.member (DC dc) referredCons) $ do
    fail $ "reference for deleted data con: " ++ show dc

markDeletedTyCon :: TyCon -> Name -> SM ()
markDeletedTyCon tc n = do
  let msg = "deleted tycon: " ++ BS8.unpack n
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline
  modify' $ \env@Env{..} -> env {deletedTyCons = Set.insert n deletedTyCons}
  -- check if a referred is deleted
  Env{..} <- get
  when (Set.member (TC tc) referredTyCons) $ do
    fail $ "reference for deleted ty con: " ++ show tc

dummyBinder :: Binder
dummyBinder = Binder
  { binderName      = "nonTermination"
  , binderId        = BinderId (Unique '-' 0)
  , binderType      = SingleValue LiftedRep
  , binderTypeSig   = "SomeException"
  , binderScope     = HaskellExported
  , binderDetails   = VanillaId
  , binderInfo      = ""
  , binderDefLoc    = UnhelpfulSpan $ UnhelpfulOther "strip-dead-code"
  , binderUnitId    = UnitId "base"
  , binderModule    = ModuleName "Control.Exception.Base"
  , binderTopLevel  = True
  -- optimization
  , binderUniqueName  = "base_Control.Exception.Base.nonTermination"
  , binderUNameHash   = hash ("base_Control.Exception.Base.nonTermination" :: Name)
  }

data StripStat
  = StripStat
  { ssDeletedDefs   :: [Name]
  , ssDeletedRefs   :: [Name]
  , ssDeletedData   :: [Name]
  , ssDeletedAlts   :: [Name]
  , ssDeletedCons   :: [Name]
  , ssDeletedTyCons :: [Name]
  }

data LivenessFacts
  = LivenessFacts
  { lfReachableCode   :: Set Name
  , lfLiveStaticData  :: Set Name
  , lfLiveConstructor :: Set Name
  , lfLiveConGroup    :: Set Name
  }

initDataConMap :: [(UnitId, [(ModuleName, [TyCon])])] -> SM ()
initDataConMap tyCons = do
  let cons :: [DataCon]
      cons = concatMap tcDataCons . concatMap snd . concatMap snd $ tyCons
  modify' $ \env -> env {dataConMap = Map.fromList [(dcId dc , dc) | dc <- cons]}

stripDeadCode :: LivenessFacts -> (Map Name Name, Map Name [Name]) -> Module -> IO (Maybe (Module, StripStat))
stripDeadCode LivenessFacts{..} (idBndMap, caseResultMap) stgMod =
 evalStateT (stripModule stgMod) emptyEnv where

  isLiveRhs :: Binder -> Rhs -> Bool
  isLiveRhs idBnd@Binder{..} = \case
    StgRhsCon{}
      -- HINT: keep local constructors
      | binderTopLevel == False -> True

    -- HINT: top level constructors and closures
    _ | binderScope == LocalScope || binderScope == GlobalScope
      -> case Map.lookup binderUniqueName idBndMap of
          Nothing -> error $ "internal error - name not found in binder name map: " ++ show binderUniqueName
          Just n  -> Set.member n lfReachableCode
      | otherwise
      -> Set.member binderUniqueName lfReachableCode

  isLiveAlt :: Name -> Bool
  isLiveAlt n = Set.member n lfReachableCode

  isLiveStaticData :: Name -> Bool
  isLiveStaticData n = Set.member n lfLiveStaticData

  stripModule :: Module -> SM (Maybe (Module, StripStat))
  stripModule m@Module{..} = do
    initDataConMap moduleTyCons
    -- mark all deleted top level names
    let allExternalIds = concatMap (concatMap snd . snd) moduleExternalTopIds
    forM_ (concatMap topLiftedBindings moduleTopBindings ++ allExternalIds) $ \idBnd@Binder{..} -> do
      -- setup dummy lifted symbol
      when (binderUniqueName == "base_Control.Exception.Base.nonTermination") $ do
        modify' $ \env -> env {dummyLifted = idBnd}

      -- mark top binder defined if it is reachable
      case Set.member binderUniqueName lfReachableCode of
        True  -> addDefId idBnd
        False -> do
          when (binderModule == ModuleName "GHC.Prim" && binderUnitId == UnitId "ghc-prim") $ do
            -- HINT: do not dummify GHC's builtin values, e.g void#
            addDefId idBnd

    tops <- mapM stripTopBinding moduleTopBindings
    (depList, externalIds) <- calcDependencies moduleUnitId moduleName
    let myTyCons = concat [concat [tcs | (m, tcs) <- ml, m == moduleName] | (uid, ml) <- moduleTyCons, uid == moduleUnitId]

    -- NOTE: stripTyCons must be call after stripTopBinding because it needs the populated ref sets
    strippedTyCons <- stripTyCons lfLiveConstructor lfLiveConGroup myTyCons

    Env{..} <- get

    let strippedMod = m
          { moduleTopBindings     = catMaybes tops
          , moduleExternalTopIds  = externalIds
          , moduleTyCons          = strippedTyCons
          , moduleDependency      = depList
          }

        stripStat = StripStat
          { ssDeletedDefs   = Set.toList deletedDefs
          , ssDeletedRefs   = Set.toList deletedRefs
          , ssDeletedData   = Set.toList deletedData
          , ssDeletedCons   = Set.toList deletedCons
          , ssDeletedTyCons = Set.toList deletedTyCons
          , ssDeletedAlts   = Set.toList deletedAlts
          }
    pure $ case isEmptyModule strippedMod of
      True  -> Nothing
      False -> Just (strippedMod, stripStat)


  stripTopBinding :: TopBinding -> SM (Maybe TopBinding)
  stripTopBinding tb = case tb of
    StgTopLifted b -> fmap StgTopLifted <$> stripBinding b
    StgTopStringLit idBnd stg -> case isLiveStaticData (binderUniqueName idBnd) of
      True  -> do
        addDefId idBnd
        pure $ Just tb
      False -> do
        markDeletedData (binderUniqueName idBnd)
        pure Nothing

  stripBinding :: Binding -> SM (Maybe Binding)
  stripBinding = \case
    StgNonRec idBnd rhs -> case isLiveRhs idBnd rhs of -- HINT: strip closures, keep constructors
      True -> do
        addDefId idBnd
        Just . StgNonRec idBnd <$> stripRhs rhs
      False -> do
        markDeletedDef (binderUniqueName idBnd)
        pure Nothing

    StgRec l -> do
      -- mark all deleted names before the traversal
      lives <- forM l $ \x@(idBnd, rhs) -> case isLiveRhs idBnd rhs of
        True  -> do
          addDefId idBnd
          pure $ Just x
        False -> do
          markDeletedDef (binderUniqueName idBnd)
          pure Nothing

      l' <- forM (catMaybes lives) $ \(idBnd, rhs) -> (idBnd,) <$> stripRhs rhs
      case l' of
        [] -> pure Nothing
        xs -> pure . Just $ StgRec xs

  stripRhs :: Rhs -> SM Rhs
  stripRhs rhs = case rhs of
    -- check occ ids
    StgRhsClosure capturedVars uf args e -> do
      mapM_ addDefId args
      cvars' <- mapM stripIdOcc capturedVars
      StgRhsClosure cvars' uf args <$> stripExpr e

    StgRhsCon dc args -> do
      addRefCon dc
      StgRhsCon dc <$> mapM stripArg args

  stripExpr :: Expr -> SM Expr
  stripExpr expr = case expr of

    StgCase scrutExpr scrutResult altType alts
      | [Alt AltDefault [] _] <- alts
      -> do
          addDefId scrutResult
          visitAltType altType
          -- it's a force and not a case
          StgCase <$> stripExpr scrutExpr <*> pure scrutResult <*> pure altType <*> mapM stripAlt alts

      | otherwise -> do
          addDefId scrutResult
          let altNames = case Map.lookup (binderUniqueName scrutResult) caseResultMap of
                Just l  -> l
                Nothing -> error $ "internal error - unknown case scrutinee result name: " ++ show (Id scrutResult)
          alts' <- forM (zip altNames alts) $ \(n, a) -> case isLiveAlt n of
            True  -> Just <$> stripAlt a
            False -> do
              markDeletedAlt n
              pure Nothing
          case catMaybes alts' of
            []        -> stripExpr scrutExpr -- HINT: this is a workaround, an error expression would be much better
            liveAlts  -> do
              visitAltType altType
              StgCase <$> stripExpr scrutExpr <*> pure scrutResult <*> pure altType <*> pure liveAlts

    StgLet b e -> stripBinding b >>= \case
      Just b' -> StgLet b' <$> stripExpr e
      Nothing -> stripExpr e

    StgLetNoEscape b e -> stripBinding b >>= \case
      Just b' -> StgLetNoEscape b' <$> stripExpr e
      Nothing -> stripExpr e

    StgTick t e -> StgTick t <$> stripExpr e

    -- check occ ids

    StgApp idOcc args t dbg -> StgApp <$> stripIdOcc idOcc <*> mapM stripArg args <*> pure t <*> pure dbg

    StgConApp dcOcc args t -> do
      addRefCon dcOcc
      StgConApp dcOcc <$> mapM stripArg args <*> pure t

    StgOpApp op args t tc -> StgOpApp op  <$> mapM stripArg args <*> pure t <*> pure tc

    StgLit{} -> pure expr

  visitAltType :: AltType -> SM ()
  visitAltType = \case
    AlgAlt tc -> addRefTyCon tc
    _ -> pure ()

  stripAlt :: Alt -> SM Alt
  stripAlt Alt{..} = do
    case altCon of
      AltDataCon dc -> addRefCon dc
      _ -> pure ()
    mapM_ addDefId altBinders
    Alt altCon altBinders <$> stripExpr altRHS

  stripArg :: Arg -> SM Arg
  stripArg a = case a of
    StgVarArg idOcc -> StgVarArg <$> stripIdOcc idOcc
    StgLitArg{}     -> pure a

  stripIdOcc :: Binder -> SM Binder
  stripIdOcc b@Binder{..} = do
    Env{..} <- get
    case Set.member (Id b) definedIds of
      True -> do
        addRefId b
        pure b
      False  -> do
        markDeletedRef binderUniqueName
        addRefId dummyLifted
        pure dummyLifted

dropEmpty :: [(a, [b])] -> [(a, [b])]
dropEmpty l = [v | v@(_, b) <- l, not $ null b]

stripTyCons :: Set Name -> Set Name -> [TyCon] -> SM [(UnitId, [(ModuleName, [TyCon])])]
stripTyCons liveCons liveConGroups modTyCons = do
  Env{..} <- get
  let modTyConSet     = Set.fromList $ map TC modTyCons
      refConTyConSet  = Set.fromList [TC $ uncutTyCon dcTyCon | DC DataCon{..} <- Set.toList referredCons]
      allTyCons       = [tc | TC tc <- Set.toList $ Set.unions [referredTyCons, refConTyConSet, modTyConSet]]

  strippedTyCons <- catMaybes <$> mapM (stripTyCon liveCons liveConGroups) allTyCons
  let tyConGroups = [((tcUnitId, tcModule),tc) | tc@TyCon{..} <- strippedTyCons]
  pure $ groupByUnitIdAndModule tyConGroups

{-
    done - collect referred tycons; excluding this module's tycons?
    done - add module's tycons stripped
    done - add referred tycons stripped
    done - error if a deleted tycon or con is referred
-}

stripTyCon :: Set Name -> Set Name -> TyCon -> SM (Maybe TyCon)
stripTyCon liveCons liveConGroups tc@TyCon{..} = do
  case Set.member tcUniqueName liveConGroups of
    False -> do
      markDeletedTyCon tc $ tcUniqueName <> " " <> BS8.pack (show tcId)
      forM_ tcDataCons $ \dc@DataCon{..} -> do
        markDeletedCon dc $ dcUniqueName <> " " <> BS8.pack (show dcId) <> " [whole tycon]"
      pure Nothing
    True -> do
      strippedCons <- forM tcDataCons $ \dc@DataCon{..} -> do
        case Set.member dcUniqueName liveCons of
          False -> do
            markDeletedCon dc $ dcUniqueName <> " " <> BS8.pack (show dcId) <> " [single con]"
            pure Nothing
          True -> pure $ Just dc
      case catMaybes strippedCons of
        [] -> do
          markDeletedTyCon tc $ tcUniqueName <> " " <> BS8.pack (show tcId)
          pure Nothing
        dcList -> pure . Just $ tc {tcDataCons = dcList}

calcDependencies :: UnitId -> ModuleName -> SM ([(UnitId, [ModuleName])], [(UnitId, [(ModuleName, [Binder])])])
calcDependencies unitId modName = do
  {-
      calc deps according to
        done - referred ids
        done - referred tycons
        done - referred cons
  -}
  Env{..} <- get
  -- referredIds
  let extIds    = [ ((binderUnitId, binderModule), b)
                  | Id b@Binder{..} <- Set.toList referredIds
                  , binderUnitId /= unitId || binderModule /= modName
                  ]
      extGroups = groupByUnitIdAndModule extIds

      -- ty con deps
      extTyCons = [ (tcUnitId, tcModule)
                  | TC tc@TyCon{..} <- Set.toList referredTyCons
                  , tcUnitId /= unitId || tcModule /= modName
                  ]
      -- con deps
      extCons   = [ (dcUnitId, dcModule)
                  | DC dc@DataCon{..} <- Set.toList referredCons
                  , dcUnitId /= unitId || dcModule /= modName
                  ]

      extDeps :: [(UnitId, ModuleName)]
      extDeps = map fst extIds ++ extTyCons ++ extCons

      deps    = map (fmap (map fst)) . groupByUnitIdAndModule $ zip extDeps (repeat ())

  pure (deps, extGroups)

----------

topLiftedBindings :: TopBinding' idBnd idOcc dcOcc tcOcc -> [idBnd]
topLiftedBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> map fst bs
  StgTopStringLit _ _           -> []

groupByUnitIdAndModule :: Ord b => [((UnitId, ModuleName), b)] -> [(UnitId, [(ModuleName, [b])])]
groupByUnitIdAndModule l =
  Map.toList . fmap (Map.toList . fmap Set.toList) $
  Map.unionsWith (Map.unionWith Set.union)
  [Map.singleton u (Map.singleton m (Set.singleton b)) | ((u, m), b) <- l]

isEmptyModule :: Module -> Bool
isEmptyModule Module{..} =
  null [tc | (u, ml) <- moduleTyCons, u == moduleUnitId, (m, tcl) <- ml, m == moduleName, tc <- tcl] &&
  null moduleTopBindings &&
  null moduleForeignFiles &&
  NoStubs /= moduleForeignStubs

{-
  TODO:
    done - strip unused externals
    done - strip constructors
    done - strip static data
    done - return Nothing if the stripped module is empty

  recalculate:
    done - moduleDependency          :: ![(UnitId, [ModuleName])]
    done - moduleExternalTopIds      :: ![(UnitId, [(ModuleName, [idBnd])])]
    done - moduleTyCons              :: ![(UnitId, [(ModuleName, [tcBnd])])]
    done - moduleTopBindings         :: ![TopBinding' idBnd idOcc dcOcc tcOcc]
    done - moduleForeignFiles        :: ![(ForeignSrcLang, FilePath)]

  fix:
    done - do not remove void tokens
    done - type correct dummification ; now only lifted binders are deleted
    done - remove special dummification support from ext stg interpreter
    - delete unused constructor allocations
    done - delete unused closure allocations
-}
