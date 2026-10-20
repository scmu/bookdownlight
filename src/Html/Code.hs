{-# LANGUAGE OverloadedStrings #-}
module Html.Code where

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Map as Map

--import Data.Text.ICU
--import Data.Text.ICU.Replace
import Text.Replace
-- import Text.Megaparsec
-- import Replace.Megaparsec
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Config (FileRole(..), relPathToFile)
import Html.Types
import Html.RenderMonad (mkTagAttrsC)

formatLineCode :: Text -> Text
formatLineCode =
  L.toStrict . inInlineCodeL . replaceWithTrie blockTrie . L.fromStrict

formatBlockCode :: (FileRole, LblMap) -> Text -> Text
formatBlockCode ctxt = formatComments ctxt .
  L.toStrict . replaceWithTrie blockTrie . L.fromStrict


formatCodeInMath :: Text -> Text
formatCodeInMath = L.toStrict . replaceWithTrie blockTrie . L.fromStrict .
                   handleLineCode "" ""

blockTrie = listToTrie . map (uncurry Replace) $ blockReplaces

blockReplaces =
   [ ("<", "&lt;")
    ,(">", "&gt;")

    ,("{-\"~~,\"-}", "")
    ,("{-\"~~.\"-}", "")
    ,("{-\"~\"-}", " ")
    ,("{-\"\\,\"-}", " ")
    ,("{-\"~\\,\"-}", " ")
    ,("\\mbox{~~.}", "")

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

    ,("forall ", "∀ ")
    ,("exists ", "∃ ")

    ,("`otimes`", "⊗")
    ,("`oplus`",  "⊕")
    ,("`odot`",   "⊙")
    ]

handleLineCode :: Text -> Text -> Text -> Text
handleLineCode opn cls txt = case txt =~ ("\\|+" :: Text) :: (Text, Text, Text) of
  (_, "", _) -> txt
  (pre, "|", more) ->  pre <> opn <>
                         inInlineCode cls more
  (pre, "||", rest) -> pre <> "|" <> handleLineCode opn cls rest
  (pre, "||||", rest) -> pre <> "⋁" <> handleLineCode opn cls rest
  (pre, other, rest) -> pre <> other <> handleLineCode opn cls rest
 where
   inInlineCode :: Text -> Text -> Text
   inInlineCode cls txt = case txt =~ ("\\|+" :: Text) :: (Text, Text, Text) of
     (_, "", _) -> txt
     (pre, "|", more) ->  pre <> cls <> handleLineCode opn cls more
     (pre, "||", rest) -> pre <> "|" <> inInlineCode cls rest
     (pre, "||||", rest) -> pre <> "⋁" <> inInlineCode cls rest
     (pre, other, rest) -> pre <> other <> inInlineCode cls rest

inInlineCodeL :: L.Text -> L.Text
inInlineCodeL txt = case txt =~ ("\\|+" :: L.Text) :: (L.Text, L.Text, L.Text) of
  (_, "", _) -> txt
  (pre, "||", rest) -> pre <> "|" <> inInlineCodeL rest
  (pre, "||||", rest) -> pre <> "⋁" <> inInlineCodeL rest
  (pre, other, rest) -> pre <> other <> inInlineCodeL rest

formatComments :: (FileRole, LblMap) -> Text -> Text
formatComments ctxt = wrapComments . map hdlLineCode . extractComments
  where wrapComments [] = ""
        wrapComments [Left txt] = txt
        wrapComments [Right txt] = "<comment>{-" <> txt <> "-}</comment>"
        wrapComments (Left txt : rest) = txt <> wrapComments rest
        wrapComments (Right txt : rest) =
          "<comment>{-" <> txt <> "-}</comment>" <> wrapComments rest
        hdlLineCode seg@(Left _) = seg
        hdlLineCode (Right txt) =
           Right (handleRefs ctxt (handleLineCode "<code class=\"haskell\">" "</code>" txt))

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

handleRefs :: (FileRole, LblMap) -> Text -> Text
handleRefs (this, lmap) = T.concat . map hdlRef . extractRefs
  where hdlRef (Left txt) = txt
        hdlRef (Right lbl) = case Map.lookup lbl lmap of
          Nothing -> "[RefUndefined]"
          Just (ch, nums) ->
              let href = mkRef (ch, nums) lbl
              in mkHLink href (secNums nums)

        mkRef (ch, nums) lbl
          | this == Chap ch  = T.cons '#' lbl
          | otherwise =
               let fname = relPathToFile this (Chap ch)
               in T.pack fname <> (T.cons '#' lbl)

extractRefs :: Text -> [Either Text Text]
extractRefs txt = case txt =~ ("\\\\ref{[^}]*}" :: Text) :: (Text, Text, Text) of
  (_, "", _) -> [Left txt]
  (pre, ref, rest) -> Left pre : Right (getLbl ref ): extractRefs rest
 where getLbl :: Text -> Text
       getLbl = T.drop 5 . T.init

-- Unfortunately we have to define pure versions
-- of some functions.

  -- a pure version of Html.RenderMonad.mkTagAttrsC

mkHLink :: Text -> Text -> Text
mkHLink href txt =
  "<a href=\"" <> href <> "\">" <> txt <> "</a>"

  -- a pure version of Html.Render.printSecNum

secNums :: [Int] -> Text
secNums [-1]   = ""
secNums ns     = pSN ns
 where pSN []     = ""
       pSN [i]    = T.pack (show i)
       pSN (i:is) = T.pack (show i) <> T.cons '.' (pSN is)
