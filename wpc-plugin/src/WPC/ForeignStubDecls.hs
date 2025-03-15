module WPC.ForeignStubDecls where

import           Data.Monoid                   (Monoid (..))

import           GHC.Hs.Extension              (GhcTc)
import           GHC.Plugins                   (FastString, Id)
import           GHC.Types.ForeignStubs        (ForeignStubs (..))

import           Language.Haskell.Syntax.Decls (ForeignExport, ForeignImport)

import           Prelude                       (Bool, Int, Maybe (..), String)

-- | Foreign export stub detailed declarations
newtype ForeignStubDecls = ForeignStubDecls [(ForeignStubs, StubDecl)]

data StubImpl
  = StubImplImportCWrapper
  { siCWrapperLabel  :: FastString
  , siStdCallArgSize :: (Maybe Int) -- arg list size for std call mangling
  , siIsIOCall       :: Bool
  , siReturnType     :: String
  , siArgTypes       :: [String]
  }

data StubDecl
  = StubDeclImport (ForeignImport GhcTc) (Maybe StubImpl)
  | StubDeclExport (ForeignExport GhcTc) Id               -- HINT: exported HsId

mergeForeignStubs :: [ForeignStubs] -> ForeignStubs
mergeForeignStubs stubs = case [(h, c) | ForeignStubs h c <- stubs] of
  [] -> NoStubs
  l  -> ForeignStubs h c where (h, c) = mconcat l

