{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module WPC.Yaml where

import GHC.Prelude
import GHC.Utils.Json
import GHC.Utils.Outputable

renderYAML :: JsonDoc -> SDoc
renderYAML d =
  case d of
    JSNull      -> text "null"
    JSBool b    -> text $ if b then "true" else "false"
    JSInt n     -> ppr n
    JSString s  -> doubleQuotes $ text $ escapeJsonString s
    JSArray l   -> vcat [ if isContainer value
                            then text "-" $+$ nest 2 (renderYAML value)
                            else text "-" <+> renderYAML value
                        | value <- l
                        ]
    JSObject l  -> vcat [ if isContainer value
                            then text key <> colon $+$ nest 2 (renderYAML value)
                            else text key <> colon <+> renderYAML value
                        | (key, value) <- l
                        ]
  where
    isContainer = \case
      JSArray{}   -> True
      JSObject{}  -> True
      _           -> False
