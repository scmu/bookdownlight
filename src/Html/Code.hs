{-# LANGUAGE OverloadedStrings #-}
module Html.Code where

import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Html.Types

--import Data.Text.ICU
--import Data.Text.ICU.Replace
import Text.Replace

formatCode :: Text -> Text
formatCode =
    L.toStrict . replaceWithList replaces . L.fromStrict
  where replaces = map (uncurry Replace)
          [("<", "&lt;")
          ,(">", "&gt;")

          ,("{-\"~~,\"-}", "")
          ,("{-\"~~.\"-}", "")
          ,("{-\"~\"-}", " ")
          ,("{-\"~\\,\"-}", " ")

          ,(":*", "×")
          ,("`min`", "↓")
          ,("`max`", "↑")
          ,("-:", "-")
          ,("+:", "+")
          ,("`sse`", "⊆")
          ,("`mem`", "∈")
          ,("`union`", "∪")
          ,("`intersect`", "∩")
          ]
