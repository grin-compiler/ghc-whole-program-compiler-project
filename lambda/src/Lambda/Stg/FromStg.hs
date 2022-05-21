{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverloadedStrings #-}
module Lambda.Stg.FromStg (codegenLambda, CGStat(..)) where

import Data.List (intercalate, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.State.Strict
import Text.Printf
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Control.Monad.Trans.Maybe

import Data.Functor.Foldable
import qualified Data.Foldable
import Transformations.Util

-- External STG
import qualified Stg.Syntax as C
import Stg.Reconstruct (topBindings)
import qualified Stg.Pretty as C
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- Lambda
import Lambda.Syntax
import Lambda.Pretty
import Lambda.Util
import Lambda.Name

import qualified Lambda.Stg.GHCPrimOps as GHCPrim
import qualified Lambda.GHC.RtsAbstractModel as GHCRts

type CG = StateT Env IO

data Env
  = Env
  { externals     :: !(Map Name External)
  , defs          :: ![Def]
  , staticData    :: ![StaticData]

  -- bind chain handling
  , commands      :: ![Cmd]
  , commandStack  :: ![[Cmd]]

  -- derive new name related and defined names
  , namePool      :: !(Map Name Int)
  , nameSet       :: !(Set Name)

  -- data constructors
  , conGroupMap   :: Map Name ConGroup

  -- logging
  , errors        :: [String]
  , warnings      :: [String]
  , messages      :: [String]

  -- current module info
  , thisUnitId    :: String
  , thisModule    :: String

  -- name shadowing related
  , scopeName       :: Name             -- HINT: current scope name
  , shadowedNameMap :: !(Map Name Name) -- HINT: substitution map for shadowed names, original name -> unique name
  , scopeShadowSet  :: !(Set Name)      -- HINT: shadowed (original) names defined in the current scope

  -- code name mapping
  , codeNameMap   :: [String]
  }

emptyEnv :: Env
emptyEnv = Env
  { externals     = mempty
  , defs          = mempty
  , staticData    = mempty
  , commands      = mempty
  , commandStack  = mempty
  , namePool      = mempty
  , nameSet       = mempty
  , conGroupMap   = mempty
  , errors        = mempty
  , warnings      = mempty
  , messages      = mempty
  , thisUnitId    = ""
  , thisModule    = ""
  , scopeName       = mempty
  , shadowedNameMap = mempty
  , scopeShadowSet  = mempty
  , codeNameMap   = mempty
  }

-- utility

addBinderNameMapEntry :: C.Binder -> Name -> CG ()
addBinderNameMapEntry b name = do
  let nameMapEntry = "b\t" ++ (BS8.unpack $ C.binderUniqueName b) ++ "\t" ++ unpackName name
  modify' $ \env@Env{..} -> env { codeNameMap = nameMapEntry : codeNameMap }

addAltNameMapEntry :: C.Binder -> [Name] -> CG ()
addAltNameMapEntry b altNames = do
  let nameMapEntry = intercalate "\t" $ "a" : (BS8.unpack $ C.binderUniqueName b) : map unpackName altNames
  modify' $ \env@Env{..} -> env { codeNameMap = nameMapEntry : codeNameMap }

scopeBracket :: Name -> CG a -> CG a
scopeBracket sn action = do
  curScopeName <- gets scopeName
  curShadowMap <- gets shadowedNameMap
  curScopeShadowSet <- gets scopeShadowSet
  modify' $ \env@Env{..} ->
    env { scopeName       = sn
        , scopeShadowSet  = mempty
        }
  result <- action
  modify' $ \env@Env{..} ->
    env { scopeName       = curScopeName
        , shadowedNameMap = curShadowMap
        , scopeShadowSet  = curScopeShadowSet
        }
  pure result

refreshTyVars :: [Ty] -> CG [Ty]
refreshTyVars tys = do
  let tyVars    = Set.toList $ Set.unions [cata folder t | t <- tys]
      folder tf = foldNameTyF Set.singleton tf `mappend` Data.Foldable.fold tf

  substEnv <- forM tyVars $ \oldName -> do
    newName <- deriveNewQualifiedName oldName
    pure (oldName, newName)

  let substFun :: Ty -> Ty
      substFun t = ana (project . mapNameTy (subst $ Map.fromList substEnv)) t
  pure $ map substFun tys

addExternal :: External -> CG ()
addExternal ext = modify' $ \env@Env{..} -> env {externals = Map.insert (eName ext) ext externals}

addPrimOpExternal :: External -> CG ()
addPrimOpExternal = addExternal
{-
addPrimOpExternal ext@External{..} = do
  extMap <- gets externals
  unless (Map.member eName extMap) $ do
    -- gen fresh names
    newRetTy : newArgsTy <- refreshTyVars $ eRetType : eArgsType
    addExternal $ ext {eRetType = newRetTy, eArgsType = newArgsTy}
-}

addDef :: Def -> CG ()
addDef d = modify' $ \env -> env {defs = d : defs env}

addStaticData :: StaticData -> CG ()
addStaticData sd = modify' $ \env -> env {staticData = sd : staticData env}

-- data con handling
genDataConName :: C.DataCon -> Name
genDataConName C.DataCon{..} = mkPackageQualifiedName (BS8.unpack $ C.getUnitId dcUnitId) (BS8.unpack $ C.getModuleName dcModule) (BS8.unpack dcName)

-- name handling

deriveNewName :: Bool -> Name -> CG Name
deriveNewName isQualified name = do
  {-
    - generates unique name like: my_name.1
    - does not add to substitution map
  -}
  (newName, conflict) <- state $ \env@Env{..} ->
    let idx = Map.findWithDefault 0 name namePool
        new = if isQualified
                then mkPackageQualifiedName thisUnitId thisModule (printf "%s_%d" name idx)
                else packName (printf "%s_%d" name idx)
    in  ( (new, Set.member new nameSet)
        , env {namePool = Map.insert name (succ idx) namePool, nameSet = Set.insert new nameSet}
        )
  if conflict
    then deriveNewName isQualified name
    else pure newName

deriveNewQualifiedName :: Name -> CG Name
deriveNewQualifiedName = deriveNewName True

encodeBinderName :: C.Binder -> Name
encodeBinderName C.Binder{..}
  -- special case
  | binderId == C.rootMainBinderId = mkPackageQualifiedName "main" ":Main" "main"

  -- normal case
  | isExported      = mkPackageQualifiedName unitId modName (BS8.unpack binderName)
  | binderTopLevel  = mkPackageQualifiedName unitId modName (BS8.unpack binderName ++ "_" ++ show bu)
  | otherwise       = mkPackageQualifiedName unitId modName (BS8.unpack binderName ++ "_" ++ show bu)
--  | otherwise       = packName (BS8.unpack binderName ++ "_" ++ show bu)
  where
    C.BinderId bu = binderId
    unitId        = BS8.unpack $ C.getUnitId binderUnitId
    modName       = BS8.unpack $ C.getModuleName binderModule
    isExported    = not $ isInternalScope binderScope

defBinder :: C.Binder -> CG (Name, RepType)
defBinder b = (,) <$> defName b <*> pure (convertType $ C.binderType b)

getBinder :: C.Binder -> CG (Name, RepType)
getBinder b = (,) <$> getName b <*> pure (convertType $ C.binderType b)

getName :: C.Binder -> CG Name
getName b = do
  let originalName = encodeBinderName b
  Env{..} <- get
  unless (Set.member originalName nameSet) $ do
    fail $ "unknown name: " ++ unpackName originalName
  pure $ case Map.lookup originalName shadowedNameMap of
    Nothing           -> originalName
    Just shadowedName -> shadowedName

defName :: C.Binder -> CG Name
defName b = do
  let originalName = encodeBinderName b
  Env{..} <- get
  case Set.member originalName nameSet of
    False -> do
      -- not defined yet
      modify' $ \env@Env{..} -> env {nameSet = Set.insert originalName nameSet}
      pure originalName
    True -> case Set.member originalName scopeShadowSet of
      True  -> do
        -- already shadowed in this scope
        -- only one shadowing per scope is allowed
        reportError $ "redefinition of name: " ++ unpackName originalName ++ " in: " ++ unpackName scopeName
        pure $ originalName <> scopeName <> "_illegal_redefinition"
      False -> do
        -- not defined in the current scope yet (only in parent scope)
        shadowedName <- deriveNewName False originalName
        reportMessage $ "shadowing of name: " ++ unpackName originalName ++ " remapped to: " ++ unpackName shadowedName ++
                        " in: " ++ unpackName scopeName
        modify' $ \env@Env{..} ->
          env { nameSet         = Set.insert originalName nameSet
              , shadowedNameMap = Map.insert originalName shadowedName shadowedNameMap
              }
        pure shadowedName

-- rep type conversion

isUnboxedTuple :: BS8.ByteString -> Bool
isUnboxedTuple "ghc-prim_GHC.Prim.Solo#" = True
isUnboxedTuple name = BS8.isPrefixOf "ghc-prim_GHC.Prim.(#" name

convertType :: C.Type -> RepType
convertType = \case
  C.SingleValue r   -> SingleValue $ getPrimRep r
  C.UnboxedTuple l  -> UnboxedTuple $ map getPrimRep l
  C.PolymorphicRep  -> PolymorphicRep

getPrimRep :: C.PrimRep -> PrimRep
getPrimRep = \case
  C.Int8Rep     -> Int8Rep
  C.Int16Rep    -> Int16Rep
  C.Int32Rep    -> Int32Rep
  C.Int64Rep    -> Int64Rep
  C.IntRep      -> Int64Rep
  C.Word8Rep    -> Word8Rep
  C.Word16Rep   -> Word16Rep
  C.Word32Rep   -> Word32Rep
  C.Word64Rep   -> Word64Rep
  C.WordRep     -> Word64Rep
  C.AddrRep     -> AddrRep
  C.FloatRep    -> FloatRep
  C.DoubleRep   -> DoubleRep
  C.VoidRep     -> VoidRep
  C.LiftedRep   -> LiftedRep
  C.UnliftedRep -> UnliftedRep

-- external type conversion

getType :: C.Name -> C.PrimRep -> Maybe SimpleType
getType t = \case
  C.Int8Rep   -> Just T_Int64
  C.Int16Rep  -> Just T_Int64
  C.Int32Rep  -> Just T_Int64
  C.Int64Rep  -> Just T_Int64
  C.IntRep    -> Just T_Int64
  C.Word8Rep  -> Just T_Word64
  C.Word16Rep -> Just T_Word64
  C.Word32Rep -> Just T_Word64
  C.Word64Rep -> Just T_Word64
  C.WordRep   -> Just T_Word64
  C.AddrRep   -> Just T_Addr
  C.FloatRep  -> Just T_Float
  C.DoubleRep -> Just T_Double
  C.VoidRep   -> Just (T_Token t)
  -- NOTE:
  --  1. FFI does not support thunks and boxed types
  --  2. VecRep is not supported yet
  _ -> Nothing

showArgType :: C.Arg -> String
showArgType = \case
  C.StgLitArg l -> show $ getLitType l
  C.StgVarArg b -> show $ C.binderType b

showArgHSType :: C.Arg -> String
showArgHSType = \case
  C.StgLitArg l -> show $ getLitType l
  C.StgVarArg b -> BS8.unpack $ C.binderTypeSig b

ffiArgType :: C.Arg -> MaybeT CG Ty
ffiArgType = \case
  C.StgLitArg l -> TySimple <$> lift (deriveNewQualifiedName "t") <*> pure (getLitType l)
  C.StgVarArg b -> do
    (name, repType) <- lift $ getBinder b
    let cvtName = packName . BS8.unpack
    case C.binderType b of
      C.SingleValue C.UnliftedRep -> case BS8.words $ C.binderTypeSig b of
        -- NOTE: byte array is allowed as FFI argument ; this is GHC special case
        ["MutableByteArray#", varA] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          n1 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "MutableByteArray#" [TyCon n1 (cvtName varA) []]

        ["ByteArray#"] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "ByteArray#" []

        ["Weak#", varA] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          n1 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "Weak#" [TyCon n1 (cvtName varA) []]

        ["ThreadId#"] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "ThreadId#" []

        ["BigNat#"] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "BigNat#" []

        -- FIXME: why are these type synonyms not removed?
        ["MutableWordArray#", varA] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          n1 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "MutableWordArray#" [TyCon n1 (cvtName varA) []]

        ["WordArray#"] -> do
          n0 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 "WordArray#" []


        _ -> fail ""

      C.SingleValue t -> do
        t1 <- MaybeT . pure $ getType (C.binderTypeSig b) t
        n0 <- lift (deriveNewQualifiedName "t")
        pure $ TySimple n0 t1

      C.UnboxedTuple []
        | name `elem` ["ghc-prim_GHC.Prim.coercionToken#", "ghc-prim_GHC.Prim.realWorld#", "ghc-prim_GHC.Prim.void#"]
        -> do
          n0 <- lift (deriveNewQualifiedName "t")
          pure $ TyCon n0 name []

      _ -> fail ""
{-
      "ghc-prim_GHC.Prim.void#"           -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.realWorld#"      -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.coercionToken#"  -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.proxy#"          -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.(##)"            -> pure (SO_Builtin, Void)
-}

ffiRetType :: C.Type -> MaybeT CG Ty
ffiRetType = \case
  C.UnboxedTuple l -> mkFFIUTup l
  C.SingleValue r -> mkFFIUTup [r]
  t -> error $ "invalid FFI result value type: " ++ show t
  where
    mkFFIUTup l = do
      args <- forM (filter (/= C.VoidRep) l) $ \r ->
        TySimple <$> lift (deriveNewQualifiedName "t") <*> MaybeT (pure $ getType "" r)
      lift $ mkUnboxedTuple args

mkUnboxedTuple :: [Ty] -> CG Ty
mkUnboxedTuple args = do
  n0 <- deriveNewQualifiedName "t"
  pure $ case length args of
    0 -> TyCon n0 "ghc-prim_GHC.Prim.(##)" []
    1 -> TyCon n0 "ghc-prim_GHC.Prim.Solo#" args
    n -> TyCon n0 (packName $ "ghc-prim_GHC.Prim.(#" ++ replicate (max 0 $ n-1) ',' ++ "#)") args

-- literal conversion

getLitPrimRep :: C.Lit -> PrimRep
getLitPrimRep = \case
  C.LitChar{}     -> Word64Rep
  C.LitString{}   -> AddrRep
  C.LitNullAddr   -> AddrRep
  C.LitFloat{}    -> FloatRep
  C.LitDouble{}   -> DoubleRep
  C.LitLabel{}    -> AddrRep
  C.LitNumber t _ -> case t of
    C.LitNumInt     -> Int64Rep
    C.LitNumInt64   -> Int64Rep
    C.LitNumWord    -> Word64Rep
    C.LitNumWord64  -> Word64Rep

getLitType :: C.Lit -> SimpleType
getLitType = \case
  C.LitChar{}     -> T_Char
  C.LitString{}   -> T_Addr
  C.LitNullAddr   -> T_Addr
  C.LitFloat{}    -> T_Float
  C.LitDouble{}   -> T_Double
  C.LitLabel{}    -> T_Addr
  C.LitNumber t _ -> case t of
    C.LitNumInt     -> T_Int64
    C.LitNumInt64   -> T_Int64
    C.LitNumWord    -> T_Word64
    C.LitNumWord64  -> T_Word64

convertLit :: C.Lit -> Lit
convertLit = \case
  C.LitNumber t i -> case t of
    C.LitNumInt     -> LInt64 $ fromIntegral i
    C.LitNumInt64   -> LInt64 $ fromIntegral i
    C.LitNumWord    -> LWord64 $ fromIntegral i
    C.LitNumWord64  -> LWord64 $ fromIntegral i
  C.LitFloat   f  -> LFloat $ realToFrac f
  C.LitDouble  f  -> LDouble $ realToFrac f
  C.LitString  s  -> LString s
  C.LitChar    c  -> LChar c
  C.LitNullAddr   -> LNullAddr
  C.LitLabel l s  -> case s of
    C.FunctionLabel i -> LCodeAddr l i
    C.DataLabel       -> LDataAddr l

-- data con and ty con conversion

convertTyCons :: [(C.UnitId, [(C.ModuleName, [C.TyCon])])] -> [ConGroup]
convertTyCons tyConsGroups =
  [ mkConGroup u mod tc
  | (u, ml) <- tyConsGroups
  , (mod, tyCons) <- ml
  , tc <- tyCons
--  , not (isUnboxed tc)
  ] where
{-
      isUnboxed tc = case C.tcDataCons tc of
        [dc] | C.UnboxedTupleCon _ <- C.dcRep dc
              -> True
        _     -> False
-}

mkConGroup :: C.UnitId -> C.ModuleName -> C.TyCon -> ConGroup
mkConGroup u mod tc
  = ConGroup
  { cgName  = mkPackageQualifiedName (BS8.unpack $ C.getUnitId u) (BS8.unpack $ C.getModuleName mod) (BS8.unpack $ C.tcName tc)
  , cgCons  = map (mkConSpec tc) $ C.tcDataCons tc
  }

mkConSpec :: C.TyCon -> C.DataCon -> ConSpec
mkConSpec tc C.DataCon{..}
  = ConSpec
  { csName    = mkPackageQualifiedName (BS8.unpack $ C.getUnitId dcUnitId) (BS8.unpack $ C.getModuleName dcModule) (BS8.unpack dcName)
  , csArgsRep = case dcRep of
      C.AlgDataCon l      -> map getPrimRep l
      C.UnboxedTupleCon n -> replicate n VoidRep
  }

-- stg ast conversion

isPrimVoidRep :: Name -> Bool
isPrimVoidRep n = Set.member n voidNames where
  voidNames = Set.fromList
    [ "ghc-prim_GHC.Prim.coercionToken#"
    , "ghc-prim_GHC.Prim.realWorld#"
    , "ghc-prim_GHC.Prim.void#"
    ]

emitLitArg :: RepType -> Lit -> CG Name
emitLitArg t l = do
  name <- deriveNewQualifiedName "lit"
  emitCmd $ S (name, t, Lit l)
  pure name

visitArg :: C.Arg -> CG Name
visitArg a = fst <$> visitArgT a

visitArgT :: C.Arg -> CG (Name, RepType)
visitArgT = \case
  C.StgLitArg l -> do
    let t = SingleValue $ getLitPrimRep l
    (,t) <$> emitLitArg t (convertLit l)

  C.StgVarArg o -> do
    (name, repType) <- getBinder o
    n <- if isPrimVoidRep name
      then emitLitArg (SingleValue VoidRep) . LToken . BS8.pack . unpackName $ name
      else pure name
    pure (n, repType)

-- CG bind chain operations

type Binding = (Name, RepType, SimpleExp)
data Cmd
  = S Binding
  | L Binding
  | R [Binding]
  deriving Show

emitCmd :: Cmd -> CG ()
emitCmd c = modify' $ \env@Env{..} -> env {commands = c : commands}

openNewBindChain :: CG ()
openNewBindChain = modify' $ \env@Env{..} -> env {commands = [], commandStack = commands : commandStack}

closeBindChain :: CG (RepType, BindChain)
closeBindChain = do
  cmds <- gets commands
  modify' $ \env@Env{..} -> env {commands = head $ commandStack ++ [[]], commandStack = drop 1 commandStack}

  let mkBindChain :: Exp -> Cmd -> Exp
      mkBindChain e = \case
        S b -> LetS   [b] e
        L b -> Let    [b] e
        R l -> LetRec l   e

  pure $ case cmds of
    [S (_, t, var@Var{})] -> (t, var) -- HINT: already has Var terminator
    (S (name, t,_):_)     -> (t, foldl mkBindChain (Var name) cmds)
    _                     -> error $ "invalid bind chain: " ++ show (reverse cmds)

genResultName :: Maybe Name -> CG Name
genResultName = \case
  Just n  -> pure n
  Nothing -> deriveNewQualifiedName "result"

-- tagToEnum special case
genTagToEnum :: Name -> [C.Arg] -> Maybe C.TyCon -> CG ()
genTagToEnum resultName [arg] (Just tc) = do
  let notEnumCon ConSpec{..} = csArgsRep /= []
      tyConName = packName (BS8.unpack $ C.tcUniqueName tc)
  conMap <- gets conGroupMap
  case Map.lookup tyConName conMap of
    Nothing -> error $ "unknown TyCon name: " ++ unpackName tyConName ++ " at instruction: " ++ unpackName resultName
    Just ConGroup{..}
      | any notEnumCon cgCons
      , null cgCons
      -> error $ "invalid tagToEnum semantics (not enum ty con) at instruction: " ++ unpackName resultName
      | otherwise -> do
          arg2 <- visitArg arg
          alts <- forM (zip [0..] cgCons) $ \(tagIdx, ConSpec{..}) -> do
            altName <- deriveNewQualifiedName "tagToEnum_alt"
            conVar <- deriveNewQualifiedName "tagToEnum_con"
            let conExp = LetS [(conVar, SingleValue UnliftedRep, Con csName [])] $ Var conVar
            pure $ Alt altName (LitPat (LInt64 tagIdx)) conExp
          let ((Alt defaultAltName _ defaultExp) : alts2) = alts
              defaultAlt = Alt defaultAltName DefaultPat defaultExp
              -- QUESTION: sould the default alt raise an exception? or does GHC generate the validator code?
          emitCmd $ S (resultName, SingleValue UnliftedRep, Case arg2 $ defaultAlt : alts2)

genTagToEnum resultName _ _ = error $ "can not convert tagToEnum# at instruction: " ++ unpackName resultName

-- primop conversion
-- GHC/Stg Prim Op call conversion

primMap :: Map Name External
primMap = Map.fromList [(eName, e) | e@External{..} <- pExternals] where
  Program{..}  = GHCPrim.primPrelude

reportError :: String -> CG ()
reportError msg = do
  modify' $ \env@Env{..} -> env {errors = msg : errors}
  liftIO . P.putDoc $ P.dullred (P.text msg) P.<+> P.hardline

reportWarning :: String -> CG ()
reportWarning msg = do
  modify' $ \env@Env{..} -> env {warnings = msg : warnings}
  liftIO . P.putDoc $ P.dullgreen (P.text msg) P.<+> P.hardline

reportMessage :: String -> CG ()
reportMessage msg = do
  modify' $ \env@Env{..} -> env {messages = msg : messages}
  liftIO . P.putDoc $ P.dullcyan (P.text msg) P.<+> P.hardline

visitOpApp :: Name -> C.StgOp -> [C.Arg] -> C.Type -> Maybe C.TyCon -> CG ()
visitOpApp resultName op args ty mtc = do
  ffiTys <- runMaybeT $ do
    argsTy <- mapM ffiArgType args
    retTy <- ffiRetType ty
    pure (retTy, argsTy)
  let resultRepType = convertType ty
  case op of
    -- NOTE: tagToEnum primop is replaced with generated code
    C.StgPrimOp "tagToEnum#" -> do
      reportMessage $ "replacing tagToEnum# at " ++ unpackName resultName
      genTagToEnum resultName args mtc

    C.StgPrimOp prim -> do
      let name = packName (BS8.unpack prim)
      case Map.lookup name primMap of
        Nothing -> do
          let errMsg = "Unsupported GHC primop: " ++ BS8.unpack prim ++ " return type: " ++ show ty
              errLit = Lit . LError $ BS8.pack errMsg
          reportError errMsg
          emitCmd $ S (resultName, resultRepType, errLit)

        Just e  -> do
          addPrimOpExternal e
          args2 <- mapM visitArg args
          emitCmd $ S (resultName, resultRepType, App name args2)

    C.StgPrimCallOp p@(C.PrimCall labelName uid) -> case ffiTys of
      -- TODO: allow any argument type for prim calls, they are staying in the managed RTS area
      Just (retTy, argsTy) -> do
        let name = mkPackageQualifiedName (BS8.unpack $ C.getUnitId uid) "" (BS8.unpack labelName)
        addExternal External
          { eName       = name
          , eRetType    = retTy
          , eArgsType   = argsTy
          , eEffectful  = False
          , eKind       = PrimOp
          }
        args2 <- mapM visitArg args
        emitCmd $ S (resultName, resultRepType, App name args2)

      _ -> do
        let name      = BS8.unpack labelName
            argsTy    = map showArgType args
            argsHSTy  = map showArgHSType args
            retTy     = show ty
            errMsg    = unlines
                          [ "Unsupported foreign primitive type: " ++ name ++ " :: " ++ intercalate " -> " (argsTy ++ [retTy])
                          , "Unsupported foreign primitive type: " ++ name ++ " :: " ++ intercalate " -> " (argsHSTy ++ [retTy])
                          ]
            errLit    = Lit . LError . BS8.pack $ errMsg
        reportError errMsg
        emitCmd $ S (resultName, resultRepType, errLit)

    C.StgFCallOp f@C.ForeignCall{..} -> case foreignCTarget of
      C.DynamicTarget -> do
        let (fnTy:argsTy)     = map showArgType args
            (fnHSTy:argsHSTy) = map showArgHSType args
            retTy             = show ty
            errMsg            = unlines
                                  [ "DynamicTarget is not supported: (" ++ fnTy ++ ") :: " ++ intercalate " -> " (argsTy ++ [retTy])
                                  , "DynamicTarget is not supported: (" ++ fnHSTy ++ ") :: " ++ intercalate " -> " (argsHSTy ++ [retTy])
                                  ]
            errLit            =  Lit . LError . BS8.pack $ errMsg
        reportError errMsg
        emitCmd $ S (resultName, resultRepType, errLit)

      C.StaticTarget _ labelName _ _ -> case ffiTys of
        Just (retTy, argsTy) -> do
          let name = packName $ BS8.unpack labelName
          addExternal External
            { eName       = name
            , eRetType    = retTy
            , eArgsType   = argsTy
            , eEffectful  = True
            , eKind       = FFI
            }
          args2 <- mapM visitArg args
          emitCmd $ S (resultName, resultRepType, App name args2)

        _ -> do
          let name      = BS8.unpack labelName
              argsTy    = map showArgType args
              argsHSTy  = map showArgHSType args
              retTy     = show ty
              errMsg    = unlines
                            [ "Unsupported foreign function type: " ++ name ++ " :: " ++ intercalate " -> " (argsTy ++ [retTy])
                            , "Unsupported foreign function type: " ++ name ++ " :: " ++ intercalate " -> " (argsHSTy ++ [retTy])
                            ]
              errLit    = Lit . LError . BS8.pack $ errMsg
          reportError errMsg
          emitCmd $ S (resultName, resultRepType, errLit)

{-
  always generate result var for expr simple exps
  the redundant result = var operation will be removed in closeBindChain operation
-}
-- this builds a bind chain
visitExpr :: Maybe Name -> C.Expr -> CG ()
visitExpr mname expr = case expr of
  -- S item
  -- generate result var if necessary
  C.StgLit lit -> do
    name <- genResultName mname
    emitCmd $ S (name, SingleValue $ getLitPrimRep lit, Lit $ convertLit lit)

  -- S item
  C.StgApp var [] _ _ -> do
    (n, t) <- getBinder var
    name <- genResultName mname
    case t of
      SingleValue LiftedRep -> do
        -- NOTE: force thunk
        emitCmd $ S (name, SingleValue LiftedRep, App n [])

      _ | C.JoinId x <- C.binderDetails var
        -> do
          unless (x == 0) $ do
            reportWarning $ "join-id var arity error, expected 0, got: " ++ show x ++ " id: " ++ show var
          -- HINT: call join id
          emitCmd $ S (name, t, App n [])

      _ | isPrimVoidRep n
        -> do
          emitCmd $ S (name, SingleValue VoidRep, Lit . LToken . BS8.pack . unpackName $ n)

      _ -> do
        emitCmd $ S (name, t, Var n)

  -- S item
  -- generate result var if necessary
  C.StgApp fun args t _ -> do
    fun2 <- getName fun
    args2 <- mapM visitArg args
    name <- genResultName mname
    emitCmd $ S (name, convertType t, App fun2 args2)

  -- S item
  -- generate result var if necessary
  C.StgConApp con args _ -> do
    (args2, argsTy) <- unzip <$> mapM visitArgT args
    let con2 = genDataConName con
        t = if isUnboxedTuple (BS8.pack $ unpackName con2) then UnboxedTuple (map utItemTy argsTy) else SingleValue UnliftedRep
        utItemTy (SingleValue i) = i
        utItemTy i = error $ "invalid unboxed tuple argument type: " ++ show i
    name <- genResultName mname
    emitCmd $ S (name, t, Con con2 args2)

  -- S item
  -- generate result var if necessary
  C.StgOpApp op args ty mtc -> do
    name <- genResultName mname
    visitOpApp name op args ty mtc

  C.StgCase scrutExpr scrutResult _ alts  -> do
    -- collect scrutinee Cmds
    -- caseses
      -- pattern match: emit case an create alts ; generate result var if necessary
      -- eval: continue building the binding chain (default rhs) ; no need for result var because binding chain continues
    (scrutName, scrutType) <- defBinder scrutResult
    visitExpr (Just scrutName) scrutExpr
    case alts of
      -- NOTE: force convention in STG
      [C.Alt C.AltDefault [] rhsExpr] -> visitExpr mname rhsExpr
{-
      -- NOTE: effectful operation return convention in STG
      [C.Alt C.AltDataCon{} [] rhsExpr]
        | scrutType == UnboxedTuple []
        -> visitExpr mname rhsExpr
-}
      -- normal case
      _ -> do
        altNames <- forM alts $ \_ -> deriveNewQualifiedName "alt"
        addAltNameMapEntry scrutResult altNames
        lamAlts <- forM (zip altNames alts) $ \(altName, alt) -> do
          visitAlt altName alt
        let (altResultRepTypes, alts2) = unzip lamAlts
        name <- genResultName mname
        ty <- case joinRepTypes ("case scrut: " ++ show (PP scrutResult)) altResultRepTypes of
          Right ty  -> pure ty
          Left err  -> do
            reportWarning err
            pure Auto
        emitCmd $ S (name, ty, Case scrutName alts2)


  ---------------------------
  -- IMPORTANT: let binder is a Con or Closure, so it does not affect the current bind chain!!!!!
  ---------------------------
  -- L item ; no need for result var because binding chain continues
  C.StgLet (C.StgNonRec b r) e -> do
    name <- defName b
    addBinderNameMapEntry b name
    scopeBracket name $ do
      (t, exp) <- visitRhs r
      emitCmd $ L (name, t, exp)
      visitExpr mname e

  -- R item ; no need for result var because binding chain continues
  C.StgLet (C.StgRec bs) e -> do
    bs1 <- forM bs $ \(b, r) -> do
      name <- defName b
      addBinderNameMapEntry b name
      pure (name, r)
    bs2 <- forM bs1 $ \(name, r) -> scopeBracket name $ do
      (t, exp) <- visitRhs r
      pure (name, t, exp)
    emitCmd $ R bs2
    visitExpr mname e

  -- L item ; no need for result var because binding chain continues
  C.StgLetNoEscape (C.StgNonRec b r) e -> do
    name <- defName b
    addBinderNameMapEntry b name
    scopeBracket name $ do
      (t, exp) <- visitRhs r
      emitCmd $ L (name, t, exp)
      visitExpr mname e

  -- R item ; no need for result var because binding chain continues
  C.StgLetNoEscape (C.StgRec bs) e -> do
    bs1 <- forM bs $ \(b, r) -> do
      name <- defName b
      addBinderNameMapEntry b name
      pure (name, r)
    bs2 <- forM bs1 $ \(name, r) -> scopeBracket name $ do
      (t, exp) <- visitRhs r
      pure (name, t, exp)
    emitCmd $ R bs2
    visitExpr mname e

  C.StgTick _ e -> visitExpr mname e

  _ -> error . printf "unsupported expr %s" $ show expr

visitAlt :: Name -> C.Alt -> CG (RepType, Alt)
visitAlt altName (C.Alt altCon argIds body) = do
  scopeBracket altName $ do
    openNewBindChain
    cpat <- case altCon of
      C.AltDataCon dc  -> NodePat (genDataConName dc) <$> mapM defName argIds
      C.AltLit lit     -> pure . LitPat $ convertLit lit
      C.AltDefault     -> pure DefaultPat
    -- bind chain
    visitExpr Nothing body
    (rt, body2) <- closeBindChain
    pure (rt, Alt altName cpat body2)

joinRepTypes :: String -> [RepType] -> Either String RepType
joinRepTypes msg = foldM f Auto where
  f (SingleValue LiftedRep) (SingleValue UnliftedRep) = pure $ SingleValue LiftedRep
  f (SingleValue UnliftedRep) (SingleValue LiftedRep) = pure $ SingleValue LiftedRep
  f (SingleValue a) (UnboxedTuple [b])                = f (SingleValue a) (SingleValue b) -- Q: why do we need this?
  f (UnboxedTuple [a]) (SingleValue b)                = f (SingleValue a) (SingleValue b) -- Q: why do we need this?
  f (UnboxedTuple []) (UnboxedTuple [VoidRep])        = pure $ UnboxedTuple []
  f (UnboxedTuple [VoidRep]) (UnboxedTuple [])        = pure $ UnboxedTuple []
{-
  f (UnboxedTuple a) (UnboxedTuple b)
    | fa <- filter (/=VoidRep) a
    , fb <- filter (/=VoidRep) b
    , fa == fb
    = UnboxedTuple fa
-}
  f Auto b = pure b
  f a Auto = pure a
  f a b
    | a == b    = pure a
    | otherwise = Left $ "can not join RepType: " ++ show (PP a, PP b) ++ "\n" ++ msg

visitRhs :: C.Rhs -> CG (RepType, SimpleExp)
visitRhs = \case
  C.StgRhsCon con args -> do
    c <- Con (genDataConName con) <$> mapM visitArg args
    pure (SingleValue LiftedRep, c)

  C.StgRhsClosure _ _ bs body -> do
    openNewBindChain
    bs2 <- mapM defBinder bs
    visitExpr Nothing body
    (_, body2) <- closeBindChain
    pure (SingleValue LiftedRep, Closure [] bs2 body2)

visitTopRhs :: C.Binder -> C.Rhs -> CG ()
visitTopRhs b = \case
  C.StgRhsClosure _ _ bs body -> do
    name <- getName b
    addBinderNameMapEntry b name
    scopeBracket name $ do
      openNewBindChain
      params <- mapM defBinder bs
      visitExpr Nothing body
      (_, body2) <- closeBindChain
      addDef (Def name params body2)

  C.StgRhsCon con args -> do
    openNewBindChain
    name <- getName b
    addBinderNameMapEntry b name
    resultVar <- deriveNewQualifiedName "result"
    con2 <- Con (genDataConName con) <$> mapM visitArg args
    emitCmd $ S (resultVar, SingleValue LiftedRep, con2)
    (_, body) <- closeBindChain
    addDef $ Def name [] body

visitTopBinder :: C.TopBinding -> CG ()
visitTopBinder = \case
  C.StgTopStringLit b s -> do
    name <- getName b
    addBinderNameMapEntry b name
    addStaticData $ StaticData name (StaticString s)

  C.StgTopLifted (C.StgNonRec b r) -> do
    visitTopRhs b r

  C.StgTopLifted (C.StgRec bs) -> do
    mapM_ (uncurry visitTopRhs) bs

{-
  rewrite and do on the fly:
    done - lit arg to name
    done - var terminator
    done - rep type on binding site
    done - convert directly to Lambda2

  idea:
    walk ext stg and collect values and expressions in state monad
-}

isInternalScope :: C.Scope -> Bool
isInternalScope = \case
  C.HaskellExported -> False
  C.ForeignExported -> False
  _ -> True

visitModule :: C.Module -> CG ([Name], [Name])
visitModule C.Module{..} = do
  -- setup module info
  let unitId  = BS8.unpack $ C.getUnitId moduleUnitId
      modName = BS8.unpack $ C.getModuleName moduleName
  modify' $ \env ->
    env { thisUnitId = unitId
        , thisModule = modName
        }
  -- register top level names
  let (internalIds, exportedIds)      = partition (isInternalScope . C.binderScope) $ concatMap topBindings moduleTopBindings
      (exportedIdsHS, exportedIdsFFI) = partition (\b -> C.binderScope b == C.HaskellExported) exportedIds
  mapM_ defName internalIds
  exportedTopNamesHS <- mapM defName exportedIdsHS
  exportedTopNamesFFI <- mapM defName exportedIdsFFI
  -- register external names
  extNames <- mapM defName (concatMap snd $ concatMap snd moduleExternalTopIds)
  -- convert bindings
  mapM_ visitTopBinder moduleTopBindings

  -- HACK: add a super main function for the Main module
  when (unitId == "main" && modName == "Main") $ do
    addRtsModel

  -- return public names: external top ids + exported top bindings
  pure (extNames ++ exportedTopNamesHS, exportedTopNamesFFI)

codegenLambda :: C.Module -> IO (Program, CGStat)
codegenLambda mod = do
  let modName     = packName . BS8.unpack . C.getModuleName $ C.moduleName mod
      conGroups   = convertTyCons $ C.moduleTyCons mod
      initialEnv  = emptyEnv { conGroupMap = Map.fromList [(cgName c, c) | c <- conGroups] }
  ((publicNames, foreignExportedNames), Env{..}) <- runStateT (visitModule mod) initialEnv
  let finalPrg = smashLet $ Program
        { pExternals            = Map.elems externals
        , pConstructors         = conGroups
        , pPublicNames          = publicNames
        , pForeignExportedNames = foreignExportedNames
        , pStaticData           = staticData
        , pDefinitions          = defs
        }
      cgStat = CGStat
        { cgMessages  = messages
        , cgWarnings  = warnings
        , cgErrors    = errors
        , cgNameMap   = codeNameMap
        }
  pure (finalPrg, cgStat)

data CGStat
  = CGStat
  { cgMessages  :: [String]
  , cgWarnings  :: [String]
  , cgErrors    :: [String]
  , cgNameMap   :: [String]
  }

---------

addRtsModel :: CG ()
addRtsModel = do
  let Program{..} = GHCRts.rtsModel
  mapM_ addDef pDefinitions
  mapM_ addExternal pExternals
