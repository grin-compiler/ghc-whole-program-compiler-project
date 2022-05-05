{-# LANGUAGE LambdaCase #-}
module Stg.Foreign.Stubs where

import Stg.Program
import Stg.Syntax

genStubs :: FilePath -> IO String
genStubs ghcstgappFname = do
  mods <- getGhcStgAppModules ghcstgappFname
  let stubs         = concat $ [map genStubCode l | ForeignStubs _ _ l <- map moduleForeignStubs $ mods]
      fileIncludes  = [] -- ["#include \"HsFFI.h\""]
      code          = unlines $ filter (not . null) $ fileIncludes ++ stubs ++ extraHack
      extraHack     =
        [ "char " ++ n ++ ";"
        | n <-
            [ "RtsFlags"
            , "defaultRtsConfig"
            , "ZCMain_main_closure"
            , "n_capabilities"
            , "keepCAFs"
            -- pandoc
            , "hslua_callhsfun"
            ]
        ]
  pure code

-- gen code + collect include headers
genStubCode :: StubDecl -> String
genStubCode = \case
{-
  StubDeclImport fi (Just impl)
    | CImport _ _ mHeader spec _ <- fi
    , StubImplImportCApi wrapperName ctys@(ret : args) <- impl
    , CFunction target <- spec
    , StaticTarget _ cName _ isFun <- target
    -> let
          (_, retCType, retHsType) = ret
          (cParams, cArgs) = unzip
            [ (BS8.unpack t ++ " " ++ name, name)
            | (i, (_, t, _)) <- zip [1..] args
            , let name = "a" ++ show i
            ]
          returnCommand = case retHsType of
            'v' -> ""
            _   -> "return"
          callCommand = if isFun
                          then BS8.unpack cName ++ "(" ++ intercalate ", " cArgs ++ ")"
                          else BS8.unpack cName
          cCode = BS8.unpack retCType ++ " " ++ BS8.unpack wrapperName ++ "(" ++ intercalate ", " cParams ++ "){" ++ returnCommand ++ " " ++ callCommand ++ ";}"
          cIncludes = ["#include " ++ show h | Header _ h <- nub $ catMaybes $ mHeader : [h | (h, _, _) <- ctys]]

        in unlines $ cIncludes ++ [cCode]
  StubDeclImport _ (Just StubImplImportCWrapper{}) -> ""
  StubDeclImport _ Nothing -> ""
-}
  StubDeclImport{} -> ""
  d -> "/*\nnot implemented: " ++ show d ++ "\n*/\n"
