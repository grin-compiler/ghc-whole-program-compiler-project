{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, BangPatterns #-}
module Lambda.Stg.StripDeadCode (stripDeadCode, StripStat(..)) where

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
  { deletedNames  :: Set Name
  , dummyLifted   :: Binder
  , deletedDefs   :: Set Name
  , deletedRefs   :: Set Name
  , deletedData   :: Set Name
  , deletedAlts   :: Set Name
  , deletedCons   :: Set Name
  , deletedTyCons :: Set Name
  , referredIds     :: !(Set Id)
  , referredCons    :: !(Set DC)
  , referredTyCons  :: !(Set TC)
  }

emptyEnv :: Env
emptyEnv = Env
  { deletedNames  = Set.empty
  , dummyLifted   = dummyBinder
  , deletedDefs   = Set.empty
  , deletedRefs   = Set.empty
  , deletedData   = Set.empty
  , deletedAlts   = Set.empty
  , deletedCons   = Set.empty
  , deletedTyCons = Set.empty
  , referredIds     = Set.empty
  , referredCons    = Set.empty
  , referredTyCons  = Set.empty
  }

type SM a = StateT Env IO a

addRefId :: Binder -> SM ()
addRefId b = do
  let i = Id b
  modify' $ \env@Env{..} -> env {referredIds = if Set.member i referredIds then referredIds else Set.insert i referredIds}

addRefCon :: DataCon -> SM ()
addRefCon dc = do
  let i = DC dc
  modify' $ \env@Env{..} -> env {referredCons = if Set.member i referredCons then referredCons else Set.insert i referredCons}

addRefTyCon :: TyCon -> SM ()
addRefTyCon tc = do
  let i = TC tc
  modify' $ \env@Env{..} -> env {referredTyCons = if Set.member i referredTyCons then referredTyCons else Set.insert i referredTyCons}

markDeleted :: Name -> SM ()
markDeleted n = do
  modify' $ \env@Env{..} -> env {deletedNames = Set.insert n deletedNames}

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
  { binderName      = "lifted_dummy"
  , binderId        = BinderId (Unique '-' 0)
  , binderType      = SingleValue LiftedRep
  , binderTypeSig   = "forall a . a"
  , binderScope     = HaskellExported
  , binderDetails   = VanillaId
  , binderInfo      = ""
  , binderDefLoc    = UnhelpfulSpan "strip-dead-code"
  , binderUnitId    = UnitId "strip-dead-code"
  , binderModule    = ModuleName "Dummy"
  , binderTopLevel  = True
  -- optimization
  , binderUniqueName  = "strip-dead-code_Dummy.lifted_dummy"
  , binderUNameHash   = hash ("strip-dead-code_Dummy.lifted_dummy" :: Name)
  }

data StripStat
  = StripStat
  { ssDeletedDefs :: [Name]
  , ssDeletedRefs :: [Name]
  , ssDeletedData :: [Name]
  , ssDeletedAlts :: [Name]
  , ssDeletedCons   :: [Name]
  , ssDeletedTyCons :: [Name]
  }

stripDeadCode :: Set Name -> Set Name -> Set Name -> Set Name -> (Map Name Name, Map Name [Name]) -> Module -> IO (Maybe (Module, StripStat))
stripDeadCode liveCons liveConGroups liveStaticDataSet liveCodeSet (idBndMap, caseResultMap) stgMod =
 evalStateT (stripModule stgMod) emptyEnv where

  isLiveBinder :: Binder -> Bool
  isLiveBinder Binder{..}
    | binderScope == LocalScope || binderScope == GlobalScope
    = case Map.lookup binderUniqueName idBndMap of
        Nothing -> error $ "internal error - name not found in binder name map: " ++ show binderUniqueName
        Just n  -> Set.member n liveCodeSet
    | otherwise
    = Set.member binderUniqueName liveCodeSet

  isLiveAlt :: Name -> Bool
  isLiveAlt n = Set.member n liveCodeSet

  isLiveData :: Name -> Bool
  isLiveData n = Set.member n liveStaticDataSet

  stripModule :: Module -> SM (Maybe (Module, StripStat))
  stripModule m@Module{..} = do
    -- mark all deleted top level names
    let allExternalIds = concatMap (concatMap snd . snd) moduleExternalTopIds
    forM_ (concatMap topLiftedBindings moduleTopBindings ++ allExternalIds) $ \idBnd -> do
      let BinderId u = binderId idBnd
      case isLiveBinder idBnd of
        True  -> do
          pure ()
        False -> do
          markDeleted (binderUniqueName idBnd)

    tops <- mapM stripTopBinding moduleTopBindings
    (depList, externalIds) <- calcDependencies moduleUnitId moduleName
    let myTyCons = concat [concat [tcs | (m, tcs) <- ml, m == moduleName] | (uid, ml) <- moduleTyCons, uid == moduleUnitId]

    -- NOTE: stripTyCons must be call after stripTopBinding because it needs the populated ref sets
    strippedTyCons <- stripTyCons liveCons liveConGroups myTyCons

    Env{..} <- get

    let {-allExtIds = moduleExternalTopIds ++ [(UnitId "strip-dead-code", [(ModuleName "Dummy" , [dummyBinder])])]
        filteredExtIds = dropEmpty
          [ ( uid
            , dropEmpty [ (m, filter (\i -> Set.member (Id i) referredIds) ids)
                        | (m, ids) <- ml
                        ]
            )
          | (uid, ml) <- allExtIds
          ]

        strippedDeps = calcDependencies moduleUnitId moduleName filteredExtIds strippedTyCons
        -}

        strippedMod = m
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
    StgTopLifted b -> fmap StgTopLifted <$> stripBinding True b
    StgTopStringLit idBnd stg -> case isLiveData (binderUniqueName idBnd) of
      True  -> pure $ Just tb
      False -> do
        markDeleted (binderUniqueName idBnd)
        markDeletedData (binderUniqueName idBnd)
        pure Nothing

  stripBinding :: Bool -> Binding -> SM (Maybe Binding)
  stripBinding isTopLevel = \case
    StgNonRec idBnd rhs -> case isLiveBinder idBnd || (not isTopLevel) of
      True  -> Just . StgNonRec idBnd <$> stripRhs rhs
      False -> do
        markDeleted (binderUniqueName idBnd)
        markDeletedDef (binderUniqueName idBnd)
        pure Nothing

    StgRec l -> do
      -- mark all deleted names before the traversal
      forM_ l $ \(idBnd, _) -> do
        unless (isLiveBinder idBnd) $ do
          markDeleted (binderUniqueName idBnd)
          markDeletedDef (binderUniqueName idBnd)

      l' <- forM l $ \(idBnd, rhs) -> case isLiveBinder idBnd || (not isTopLevel) of
        True  -> Just . (idBnd,) <$> stripRhs rhs
        False -> pure Nothing
      case catMaybes l' of
        [] -> pure Nothing
        xs -> pure . Just $ StgRec xs

  stripRhs :: Rhs -> SM Rhs
  stripRhs rhs = case rhs of
    -- check occ ids
    StgRhsClosure capturedVars uf args e -> do
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
          visitAltType altType
          -- it's a force and not a case
          StgCase <$> stripExpr scrutExpr <*> pure scrutResult <*> pure altType <*> mapM stripAlt alts

      | otherwise -> do
          let altNames = case Map.lookup (binderUniqueName scrutResult) caseResultMap of
                Just l  -> l
                Nothing -> error $ "internal error - unknown case scrutinee result name: " ++ show (Id scrutResult)
          alts' <- forM (zip altNames alts) $ \(n, a) -> case isLiveAlt n of
            True  -> Just <$> stripAlt a
            False -> do
              markDeleted n
              markDeletedAlt n
              pure Nothing
          case catMaybes alts' of
            []        -> stripExpr scrutExpr -- HINT: this is a workaround, an error expression would be much better
            liveAlts  -> do
              visitAltType altType
              StgCase <$> stripExpr scrutExpr <*> pure scrutResult <*> pure altType <*> pure liveAlts

    StgLet b e -> stripBinding False b >>= \case
      Just b' -> StgLet b' <$> stripExpr e
      Nothing -> stripExpr e

    StgLetNoEscape b e -> stripBinding False b >>= \case
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
    Alt altCon altBinders <$> stripExpr altRHS

  stripArg :: Arg -> SM Arg
  stripArg a = case a of
    StgVarArg idOcc -> StgVarArg <$> stripIdOcc idOcc
    StgLitArg{}     -> pure a

  stripIdOcc :: Binder -> SM Binder
  stripIdOcc b@Binder{..} = do
    Env{..} <- get
    case Set.member binderUniqueName deletedNames of
      False -> do
        addRefId b
        pure b
      True  -> do
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
  null moduleTyCons &&
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
    - do not remove void tokens
-}
