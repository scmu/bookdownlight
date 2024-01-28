{-# LANGUAGE OverloadedStrings #-}
module Html.Code where

import Data.Void
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Html.Types

--import Data.Text.ICU
--import Data.Text.ICU.Replace
import Text.Replace
-- import Text.Megaparsec
-- import Replace.Megaparsec
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

formatLineCode :: Text -> Text
formatLineCode =
  L.toStrict . inInlineCodeL . replaceWithTrie blockTrie . L.fromStrict

formatBlockCode :: Text -> Text
formatBlockCode = formatComments .
  L.toStrict . replaceWithTrie blockTrie . L.fromStrict

blockTrie = listToTrie . map (uncurry Replace) $ blockReplaces

blockReplaces =
   [("<", "&lt;")
    ,(">", "&gt;")

    ,("{-\"~~,\"-}", "")
    ,("{-\"~~.\"-}", "")
    ,("{-\"~\"-}", " ")
    ,("{-\"\\,\"-}", " ")
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
    ,("&&", "⋀")

    ,("`otimes`", "⊗")
    ,("`oplus`",  "⊕")
    ,("`odot`",   "⊙")
    ]

handleLineCode :: Text -> Text
handleLineCode txt = case txt =~ ("\\|+" :: Text) :: (Text, Text, Text) of
  (_, "", _) -> txt
  (pre, "|", more) ->  pre <> "<code class=\"haskell\">" <>
                         inInlineCode more
  (pre, "||", rest) -> pre <> "|" <> handleLineCode rest
  (pre, "||||", rest) -> pre <> "⋁" <> handleLineCode rest
  (pre, other, rest) -> pre <> other <> handleLineCode rest

inInlineCode :: Text -> Text
inInlineCode txt = case txt =~ ("\\|+" :: Text) :: (Text, Text, Text) of
  (_, "", _) -> txt
  (pre, "|", more) ->  pre <> "</code>" <> handleLineCode more
  (pre, "||", rest) -> pre <> "|" <> inInlineCode rest
  (pre, "||||", rest) -> pre <> "⋁" <> inInlineCode rest
  (pre, other, rest) -> pre <> other <> inInlineCode rest

inInlineCodeL :: L.Text -> L.Text
inInlineCodeL txt = case txt =~ ("\\|+" :: L.Text) :: (L.Text, L.Text, L.Text) of
  (_, "", _) -> txt
  -- (pre, "|", more) ->  pre <> "</code>" <> handleLineCode more
  (pre, "||", rest) -> pre <> "|" <> inInlineCodeL rest
  (pre, "||||", rest) -> pre <> "⋁" <> inInlineCodeL rest
  (pre, other, rest) -> pre <> other <> inInlineCodeL rest

formatComments :: Text -> Text
formatComments = wrapComments . map hdlLineCode . extractComments
  where wrapComments [] = ""
        wrapComments [Left txt] = txt
        wrapComments [Right txt] = "<comment>{-" <> txt <> "-}</comment>"
        wrapComments (Left txt : rest) = txt <> wrapComments rest
        wrapComments (Right txt : rest) =
          "<comment>{-" <> txt <> "-}</comment>" <> wrapComments rest
        hdlLineCode seg@(Left _) = seg
        hdlLineCode (Right txt) = Right (handleLineCode txt)

extractComments :: Text -> [Either Text Text]
extractComments txt = case txt =~ ("{-" :: Text) :: (Text, Text, Text) of
  (_, "", _)        -> [Left txt]
  (pre, "{-", rest) -> Left pre :
                         let (cmt, rest') = grabComment rest
                         in Right cmt : extractComments rest'

grabComment :: Text -> (Text, Text)
grabComment txt =
  case txt =~ ("({-)|(-})" :: Text) :: (Text, Text, Text) of
         (_, "", _) -> (txt, "")
         (cmt, "-}", rest) -> (cmt, rest)
         (pre, "{-", more) ->
           let (cmt, rest)  = grabComment more
               (suf, rest') = grabComment rest
           in (pre <> "{-" <> cmt <> "-}" <> suf, rest')
