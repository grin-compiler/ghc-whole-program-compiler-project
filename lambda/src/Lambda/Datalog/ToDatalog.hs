{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings, RecordWildCards #-}
module Lambda.Datalog.ToDatalog where

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import qualified Data.Map as Map

import System.IO
import System.FilePath

import Control.Monad
import Control.Monad.Reader
import Data.List (intercalate, groupBy)

import Lambda.Syntax

import Transformations.Util
import Lambda.Util

type Fact = (String, [Param])
type DL = ReaderT ([Fact] -> IO ()) IO

data Param
  = S String
  | N Name
  | I Int

emit :: [Fact] -> DL ()
emit l = do
  f <- ask
  liftIO $ f l

logWriteFile :: String -> String -> IO ()
logWriteFile fname str = do
  putStrLn $ "  " ++ fname
  writeFile fname str

programToDatalogM :: FilePath -> Program -> IO ()
programToDatalogM fname prg = do
  h <- openFile fname WriteMode
  runReaderT (convertProgram prg) $ hPutStr h . toDatalog
  hFlush h
  hClose h

programToFactsM :: Bool -> FilePath -> Program -> IO ()
programToFactsM log outDir prg = do
  let factNames = [ "EvalMode", "Move", "NodeRole", "Node", "NodeArgument", "Call", "CallArgument", "IsFunction", "FunctionParameter"
                  , "Case", "Alt", "AltParameter", "IsClosure", "ClosureVariable", "ClosureParameter", "ReturnValue", "FirstInst"
                  , "NextInst", "RecGroup", "ExternalFunction", "ExternalParameterType", "ExternalReturnType", "CodeArity"
                  , "TypeNode", "TypeNodeArgument", "IsTypeVariable", "FunctionType", "FunctionTypeReturnType", "FunctionTypeParameterType"
                  , "IsStaticData", "ConGroup", "ConSpec", "ConSpecArg"
                  ]
  files <- forM factNames $ \fname -> do
    let filename = outDir </> fname ++ ".facts"
    when log $ putStrLn $ "\t" ++ filename
    h <- openFile filename WriteMode
    pure (fname ++ ".facts", h)

  let fileMap = Map.fromList files
      writeFact (f, str) = hPutStr (fileMap Map.! f) str

  runReaderT (convertProgram prg) $ mapM_ writeFact . toFacts

  forM_ (Map.elems fileMap) $ \h -> do
    hFlush h
    hClose h

toDatalog :: [Fact] -> String
toDatalog = unlines . map prettyFact where
  prettyFact :: Fact -> String
  prettyFact (n, args) = n ++ "(" ++ intercalate ", " (map showParam args) ++ ")."

  showParam :: Param -> String
  showParam = \case
    S s -> show s
    I i -> show i
    N n -> show $ unpackName n

toFacts :: [Fact] -> [(String, String)]
toFacts = map prettyFacts . Map.toList . Map.unionsWith (++) . map (\(f,a) -> Map.singleton f [a]) where
  factEq a b = fst a == fst b

  prettyFacts :: (String, [[Param]]) -> (String, String)
  prettyFacts (fname, l) = (fname ++ ".facts", unlines [intercalate "\t" (map showParam a) | a <- l])

  showParam :: Param -> String
  showParam = \case
    S s -> s
    I i -> show i
    N n -> unpackName n

convertExternal :: External -> DL ()
convertExternal External{..} = do
  retN <- convertTy eRetType
  argsN <- mapM convertTy eArgsType
  let extKind = case eKind of
        PrimOp  -> "primop"
        FFI     -> "ffi"
  emit $
    [ ("ExternalFunction", [N eName, S $ if eEffectful then "effectful" else "pure", I $ length eArgsType, S extKind])
    , ("ExternalReturnType", [N eName, N retN])
    ] ++
    [ ("ExternalParameterType", [N eName, I i, N ty]) | (i, ty) <- zip [0..] argsN]

convertTy :: Ty -> DL Name
convertTy = \case
  TyVar v -> do
    emit [("IsTypeVariable", [N v])]
    pure v
  TySimple v sty -> do
    emit [("TypeNode", [N v, S $ "lit:" ++ show sty])]
    pure v
  TyCon v n args -> do
    emit [("TypeNode", [N v, N n])]
    argsN <- mapM convertTy args
    emit [("TypeNodeArgument", [N v, I i, N p]) | (i,p) <- zip [0..] argsN]
    pure v
  TyFun n ret args -> do
    argsN <- mapM convertTy args
    retN <- convertTy ret
    emit [("FunctionType", [N n, I $ length args])]
    emit [("FunctionTypeReturnType", [N n, N retN])]
    emit [("FunctionTypeParameterType", [N n, I i, N p]) | (i,p) <- zip [0..] argsN]
    pure n

convertStaticData :: StaticData -> DL ()
convertStaticData StaticData{..} = do
  emit [("IsStaticData", [N sName])]
  case sValue of
    StaticString bs -> do
      emit [("NodeRole", [N sName, S "lit"])]
      emit [("Node", [N sName, S "T_Addr"])]
      emit [("NodeArgument", [N sName, I 0, S $ show bs])]
  pure ()

convertConGroup :: ConGroup -> DL ()
convertConGroup ConGroup{..} = do
  emit [("ConGroup", [N cgName, I (length cgCons)])]
  forM_ cgCons $ \ConSpec{..} -> do
    emit [("ConSpec", [N cgName, N csName, I (length csArgsRep)])]
    forM_ (zip [0..] csArgsRep) $ \(idx, rep) -> do
      emit [("ConSpecArg", [N csName, I idx, S (show rep)])]

convertProgram :: Exp -> DL ()
convertProgram = \case
  Program{..} -> do
    mapM_ convertExternal pExternals
    mapM_ convertStaticData pStaticData
    mapM_ convertConGroup pConstructors
    -- TODO: others
    mapM_ convertDef pDefinitions
convertDef :: Exp -> DL ()
convertDef = \case
  Def n a b -> do
    emit [("IsFunction", [N n])]
    emit [("FunctionParameter", [N n, I i, N p]) | (i,(p,_t)) <- zip [0..] a]
    emit [("CodeArity", [N n, I $ length a])]
    -- bind
    ret <- convertBind (CodeName n) b
    emit [("ReturnValue", [N n, N ret])]

data InstInfo
  = CodeName  Name  -- first instruction's parent
  | InstName  Name  -- next instruction's parent

emitInstSeq :: InstInfo -> (Name, a, b) -> DL InstInfo
emitInstSeq i (n, _, _) = case i of
  CodeName p  -> emit [("FirstInst", [N p, N n])] >> pure (InstName n)
  InstName p  -> emit [("NextInst", [N p, N n])] >> pure (InstName n)

convertBind :: InstInfo -> BindChain -> DL Name
convertBind prevInst = \case
  Var n -> do
    pure n
  Let l b -> do
    forM_ l $ \(n,_t,e) -> do
      emit [("EvalMode", [N n, S "lazy"])]
      convertSimpleExp n e
    i <- foldM emitInstSeq prevInst l
    convertBind i b
  LetS l b -> do
    forM_ l $ \(n,_t,e) -> do
      emit [("EvalMode", [N n, S "strict"])]
      convertSimpleExp n e
    i <- foldM emitInstSeq prevInst l
    convertBind i b
  LetRec l b -> do
    forM_ l $ \(n,_t,e) -> do
      emit [("EvalMode", [N n, S "lazy"])]
      convertSimpleExp n e
    case l of
      (x,_,_) : _ -> emit [("RecGroup", [N x, N n]) | (n,_,_) <- l]
      _ -> pure ()
    i <- foldM emitInstSeq prevInst l
    convertBind i b

convertSimpleExp :: Name -> Exp -> DL ()
convertSimpleExp result = \case
  Var n -> do
    emit [("Move", [N result, N n])]

  App n a -> do
    emit [("Call", [N result, N n, I $ length a])]
    emit [("CallArgument", [N result, I i, N p]) | (i,p) <- zip [0..] a]

  Con n a -> do
    emit [("NodeRole", [N result, S "node"])]
    emit [("Node", [N result, N n])]
    emit [("NodeArgument", [N result, I i, N p]) | (i,p) <- zip [0..] a]

  Lit l -> do
    emit [("NodeRole", [N result, S "lit"])]
    emit [("Node", [N result, S $ litTag l])]
    emit [("NodeArgument", [N result, I 0, S $ show l])]

  Case n a -> do
    emit [("Case", [N result, N n])]
    mapM_ (convertAlt result) a

  Closure v p b -> do
    emit [("IsClosure", [N result])]
    emit [("ClosureVariable",  [N result, I i, N x]) | (i,x) <- zip [0..] v]
    emit [("ClosureParameter", [N result, I i, N x]) | (i,(x,_t)) <- zip [0..] p]
    emit [("CodeArity", [N result, I $ length p])]
    -- bind
    ret <- convertBind (CodeName result) b
    emit [("ReturnValue", [N result, N ret])]

  e -> error $ "Simple Expression expected, got: " ++ show e

convertAlt :: Name -> Exp -> DL ()
convertAlt result = \case
  Alt a p b -> do
    case p of
      NodePat t l -> do
        emit [("Alt", [N result, N a, N t])]
        emit [("AltParameter", [N a, I i, N p]) | (i,p) <- zip [0..] l]
      LitPat l -> do
        emit [("Alt", [N result, N a, S $ litTag l])]
      DefaultPat -> do
        emit [("Alt", [N result, N a, S "default:"])]
    -- bind
    ret <- convertBind (CodeName a) b
    emit [("ReturnValue", [N a, N ret])]

litTag :: Lit -> String
litTag l = "lit:" ++ case l of
  LInt64{}      -> "T_Int64"
  LWord64{}     -> "T_Word64"
  LFloat{}      -> "T_Float"
  LDouble{}     -> "T_Double"
  LBool{}       -> "T_Bool"
  LChar{}       -> "T_Char"
  LString{}     -> "T_String"
  LDataAddr{}   -> "T_Addr"
  LCodeAddr{}   -> "T_Addr"
  LNullAddr{}   -> "T_Addr"
  LError{}      -> "LError"
  LToken t      -> show $ T_Token t
