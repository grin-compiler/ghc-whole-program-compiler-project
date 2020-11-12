{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Rts (initRtsSupport) where

import GHC.Stack
import Control.Monad.State
import Control.Concurrent.MVar

import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

initRtsSupport :: [Module] -> M ()
initRtsSupport mods = do
  let rtsModSet = Set.fromList [(UnitId u, ModuleName m) | (u, m, _, _, _) <- wiredInCons]
      dcMap     = Map.fromList
                    [ ((moduleUnitId, moduleName, tcName, dcName), dc)
                    | m@Module{..} <- mods
                    , Set.member (moduleUnitId, moduleName) rtsModSet
                    , (tcU, tcMs) <- moduleTyCons
                    , tcU == moduleUnitId
                    , (tcM, tcs) <- tcMs
                    , tcM == moduleName
                    , TyCon{..} <- tcs
                    , dc@DataCon{..} <- tcDataCons
                    ]
      rts = foldl' setCon Rts{} wiredInCons
      setCon r (u, m, t, d, setter) = case Map.lookup (UnitId u, ModuleName m, t, d) dcMap of
        Nothing -> error $ "missing wired in data con: " ++ show (u, m, t, d)
        Just dc -> setter r dc
  modify' $ \s -> s {ssRtsSupport = rts}

-- HINT: needed for FFI value boxing
wiredInCons :: [(Name, Name, Name, Name, Rts -> DataCon -> Rts)]
wiredInCons =
  -- unit-id,     module,       type con,     data con
  [ ("ghc-prim",  "GHC.Types",  "Char",       "C#",         \s dc -> s {rtsCharCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Int",        "I#",         \s dc -> s {rtsIntCon       = dc})
  , ("base",      "GHC.Int",    "Int8",       "I8#",        \s dc -> s {rtsInt8Con      = dc})
  , ("base",      "GHC.Int",    "Int16",      "I16#",       \s dc -> s {rtsInt16Con     = dc})
  , ("base",      "GHC.Int",    "Int32",      "I32#",       \s dc -> s {rtsInt32Con     = dc})
  , ("base",      "GHC.Int",    "Int64",      "I64#",       \s dc -> s {rtsInt64Con     = dc})
  , ("ghc-prim",  "GHC.Types",  "Word",       "W#",         \s dc -> s {rtsWordCon      = dc})
  , ("base",      "GHC.Word",   "Word8",      "W8#",        \s dc -> s {rtsWord8Con     = dc})
  , ("base",      "GHC.Word",   "Word16",     "W16#",       \s dc -> s {rtsWord16Con    = dc})
  , ("base",      "GHC.Word",   "Word32",     "W32#",       \s dc -> s {rtsWord32Con    = dc})
  , ("base",      "GHC.Word",   "Word64",     "W64#",       \s dc -> s {rtsWord64Con    = dc})
  , ("base",      "GHC.Ptr",    "Ptr",        "Ptr",        \s dc -> s {rtsPtrCon       = dc})
  , ("base",      "GHC.Ptr",    "FunPtr",     "FunPtr",     \s dc -> s {rtsFunPtrCon    = dc})
  , ("ghc-prim",  "GHC.Types",  "Float",      "F#",         \s dc -> s {rtsFloatCon     = dc})
  , ("ghc-prim",  "GHC.Types",  "Double",     "D#",         \s dc -> s {rtsDoubleCon    = dc})
  , ("base",      "GHC.Stable", "StablePtr",  "StablePtr",  \s dc -> s {rtsStablePtrCon = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "True",       \s dc -> s {rtsTrueCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "False",      \s dc -> s {rtsFalseCon     = dc})
  ]

{-
        base_GHCziTopHandler_runIO_closure
        base_GHCziTopHandler_runNonIO_closure
        rts_mkString    unpackCString_closure
-}
{-
data Rts
  = Rts
  -- closures used by FFI wrapper code ; heap address of the closure
  , rtsUnpackCString       :: Atom
  , rtsTopHandlerRunIO     :: Atom
  , rtsTopHandlerRunNonIO  :: Atom
  }
  deriving (Show)
-}
