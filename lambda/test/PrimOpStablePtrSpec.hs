{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpStablePtrSpec where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.IORef
import Test.Hspec
import Test.QuickCheck
import System.IO
import Text.Show.Pretty (pPrint, ppShow)
import Text.PrettyPrint.ANSI.Leijen

import Lambda.TH
import Lambda.Analysis.ControlFlowAnalysisM
import Lambda.Pretty (PP(..))

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  ----------------------------
  usedRules <- runIO $ newIORef (Set.empty :: Set.Set [Text.Text])

  let filterAndSort keys m = fmap sort $ Map.restrictKeys m (Set.fromList keys)

      sameAs :: Show a => a -> a -> IO ()
      sameAs a b = (PP (ppShow a)) `shouldBe` (PP (ppShow b))

      toMVOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "StablePtr"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC StablePtr PrimOps" $ do

    it "makeStablePtr#" $ do
      cfa <- controlFlowAnalysisM [] ["main"] [prog|
          primop effectful
            "makeStablePtr#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"ghc-prim_GHC.Prim.Unit#" {"StablePtr#" %s.2 %a.6} @ t.109} @ t.108
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "makeStablePtr#" $ v01 v02
              v05 = case v03 of
                ("ghc-prim_GHC.Prim.Unit#" v04) @ a00 ->
                  v04
            v05
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "StablePtr" , [ [ "v01" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v05" , "v03" , "t.109" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "ghc-prim_GHC.Prim.Unit#" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "ghc-prim_GHC.Prim.Unit#" ]
            , [ "v04" , "StablePtr#" ]
            , [ "v05" , "StablePtr#" ]
            ]
          )
        ]

    it "deRefStablePtr#" $ do
      cfa <- controlFlowAnalysisM [] ["main"] [prog|
          primop effectful
            "makeStablePtr#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"ghc-prim_GHC.Prim.Unit#" {"StablePtr#" %s.2 %a.6} @ t.109} @ t.108
            "deRefStablePtr#" :: {"StablePtr#" %s.21 %a.11} @ t.243 -> {"State#" %s.21} @ t.245 -> {"ghc-prim_GHC.Prim.Unit#" %a.11} @ t.246
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "makeStablePtr#" $ v01 v02
              v09 = case v03 of
                ("ghc-prim_GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v06 = "deRefStablePtr#" $ v04 v02
                    v08 = case v06 of
                      ("ghc-prim_GHC.Prim.Unit#" v07) @ a01 ->
                        v07
                  v08
            v09
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "StablePtr" , [ [ "v01" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "a01" , "v06" , "t.246" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v06" , "v06" , "t.246" ]
            , [ "v07" , "v06" , "a.11" ]
            , [ "v08" , "v06" , "a.11" ]
            , [ "v09" , "v06" , "a.11" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v07" , "v01" ]
            , [ "v08" , "v01" ]
            , [ "v09" , "v01" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "ghc-prim_GHC.Prim.Unit#" ]
            , [ "a01" , "ghc-prim_GHC.Prim.Unit#" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "ghc-prim_GHC.Prim.Unit#" ]
            , [ "v04" , "StablePtr#" ]
            , [ "v06" , "ghc-prim_GHC.Prim.Unit#" ]
            , [ "v07" , "Tup0" ]
            , [ "v08" , "Tup0" ]
            , [ "v09" , "Tup0" ]
            ]
          )
        ]

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
