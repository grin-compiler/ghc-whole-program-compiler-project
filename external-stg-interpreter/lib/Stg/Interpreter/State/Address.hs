module Stg.Interpreter.State.Address where

import Stg.Syntax

data Addr
  = AddrInt         Int -- NOTE: only for the pretty printer
  | AddrId          Id
{-
  | AddrIdExp       {addrId :: String, addrExp :: Exp}
  | AddrIdExpKAddr  {addrId :: String, addrExp :: Exp, addrReturn :: KAddr}
  | AddrIdKAddr     {addrId :: String, addrReturn :: KAddr}
  | AddrExp         {addrExp :: Exp}
  | AddrExpEnv      {addrExp :: Exp, addrEnv :: Env}
  | AddrAAC         Exp Env Exp Env Store
  | AddrHalt
-}
  deriving (Show, Eq, Ord)

newtype AtomAddr              = AtomAddr              Addr deriving (Show, Eq, Ord)
newtype StackAddr             = StackAddr             Addr deriving (Show, Eq, Ord)
newtype HeapAddr              = HeapAddr              Addr deriving (Show, Eq, Ord)
newtype ThreadAddr            = ThreadAddr            Addr deriving (Show, Eq, Ord)
newtype StableNameAddr        = StableNameAddr        Addr deriving (Show, Eq, Ord)
newtype StablePointerAddr     = StablePointerAddr     Addr deriving (Show, Eq, Ord)
newtype WeakPointerAddr       = WeakPointerAddr       Addr deriving (Show, Eq, Ord)
newtype MutableByteArrayAddr  = MutableByteArrayAddr  Addr deriving (Show, Eq, Ord)
newtype MVarAddr              = MVarAddr              Addr deriving (Show, Eq, Ord)
newtype ArrayAddr             = ArrayAddr             Addr deriving (Show, Eq, Ord)
newtype MutableArrayAddr      = MutableArrayAddr      Addr deriving (Show, Eq, Ord)
newtype SmallArrayAddr        = SmallArrayAddr        Addr deriving (Show, Eq, Ord)
newtype SmallMutableArrayAddr = SmallMutableArrayAddr Addr deriving (Show, Eq, Ord)
newtype ArrayArrayAddr        = ArrayArrayAddr        Addr deriving (Show, Eq, Ord)
newtype MutableArrayArrayAddr = MutableArrayArrayAddr Addr deriving (Show, Eq, Ord)
newtype MutVarAddr            = MutVarAddr            Addr deriving (Show, Eq, Ord)
