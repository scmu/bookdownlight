{-# LANGUAGE OverloadedStrings #-}
module Html.Render where

import System.IO (Handle)
import qualified System.IO as IO (hPutChar, hPutStr)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text as Text (cons, append, pack)
import qualified Data.Text.IO as T (hPutStr)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Reader

import Cheapskate

import Syntax.Util

import Html.Types
import Html.Counter
import Html.RenderMonad
import Html.Pure

---- rendering

htmlRender :: Doc -> (Maybe (RMonad ()), RMonad ())
htmlRender (Doc _ blocks) =
   (fmap (\ (Header _ attrs is) -> renderHeader 1 attrs is) ***
     renderBlocks) . sepChHeader $ blocks

renderDoc :: Doc -> RMonad ()
renderDoc (Doc _ blocks) = renderBlocks blocks

renderBlocks :: Blocks -> RMonad ()
renderBlocks = mapM_ renderBlock

renderBlock :: Block -> RMonad ()
renderBlock (Para (Attrs attrs :<| is)) =
  mkTagAttrs "p" attrs' (do
    case lookupAttrs "title" attrs of
        Just title -> mkTag "b" (putStrTR title) >> putStrTR " &emsp;"
        _ -> return ()
    renderInlines is)
 where attrs' = filter (\atr -> not (isThisAtr "title"    atr ||
                                     isThisCls "noindent" atr ||
                                     isThisCls "nobreak"  atr))  attrs
renderBlock (Para is) = mkTag "p" (renderInlines is)
renderBlock (Header hd attrs is) = renderHeader hd attrs is
renderBlock (Blockquote bs) = mkTag "blockquote" (renderBlocks bs)
renderBlock (List _ lt items) =
  mkTag (ltype lt) (mapM_ renderLItem items)
 where ltype (Bullet _)     = "ul"
       ltype (Numbered _ _) = "ol"
       renderLItem bs = mkTag "li" (renderBlocks bs)
renderBlock (CodeBlock attrs txt) = renderCode (sortAttrs attrs) txt
renderBlock (DIV attrs bs)
     | []     <- cls = renderBlocks bs
     | (c:cs) <- cls = renderDIV c (cs,ids,avs) bs
  where (cls, ids, avs) = sortAttrs attrs

renderDIV :: Text -> ([Text], [Text], [(Text, Text)]) -> Blocks -> RMonad ()

renderDIV c (cs,ids,avs) bs | Just zh <- lookup c thmEnvs = do
  (_, nums) <- state newThm
  mkThmBox (cs,ids,avs)
             (do putStrTR zh >> printSecNum nums >> putCharR ' '
                 maybe (return ())
                       (\title -> putStrTR title >> putCharR ' ')
                   (lookup "title" avs))
            (renderBlocks bs)
  where thmEnvs = [("theorem",    "定理 "), ("lemma",      "引理 "),
                   ("definition", "定義 "), ("example",    "例 "),
                   ("corollary",  "系理 ")]
{-
renderDIV "figure" cs ids avs bs = do
  putStrTR "<div id='"
  mapM_ (renderLabel h) ids
  putStrTR ">\n<img src=''"
  printPositions cs
  putStrTR ">\n"
  renderBlocks bs
  printCaption avs
  putStrTR "</div>\n"
  where printPositions cs
          | [] <- ps = return ()
          | otherwise = hPutChar h '[' >> mapM_ (hPutChar h . Data.Text.head) ps >>
                       hPutChar h ']'
        ps = filter (`elem` poses) cs
          where poses = ["here", "top", "bottom", "page"]
        printCaption avs =
          case lookup "title" avs of
            Just cap -> do
              putStrTR "<figcaption>圖"
              if not $ null ids then do latexCmd h "figure" $ Prelude.head ids else do putStrTR " 0.0"
              putStrTR cap
              putStrTR "</figcaption>\n"
            Nothing -> return ()
-}
renderDIV "texonly" _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = putStrTR code >> putStrTR "<br/>\n"
        renderTexOnly b = renderBlock b

renderDIV c@"infobox" (cls, ids, avs) bs =
   mkInfoBox (c:cls, ids, avs)
      (printTitle avs) (renderBlocks bs)
 where printTitle avs = case lookup "title" avs of
         Just cap -> putStrTR cap
         Nothing -> return ()

{-
renderDIV "multicols" _ _ avs bs = do
  putStrTR "<table>\n"
  renderBlocks bs
  putStrTR "</table>\n"

renderDIV "mcol" _ _ avs bs = do
  case lookup "width" avs of
    Just w -> do putStrTR "<tr>\n"
                 --putStrTR w
                 putStrTR "<td>\n"
                 renderBlocks bs
                 putStrTR "</td>\n</tr>\n"
    Nothing -> renderBlocks bs
-}

renderDIV c@"exlist" (cs, ids, avs) bs =
  mkTagAttrsC "div" (c:cs, ids, avs)
    (renderBlocks bs)

renderDIV c@"exer" (cs, ids, avs) bs = do
  (_, nums) <- state newExer
  mkExerBox (c:cs, ids, avs)
     (putStrTR "練習 " >> printSecNum nums)
     (renderBlocks bs)

renderDIV c@"exans" (cs, ids, avs) bs = do
  counters <- currentCounters
  mkAnsBox (c:cs, ids, avs) [chC counters, exerC counters]
     (putStrTR "答案")
     (renderBlocks bs)

renderDIV c@"answer" (cs, ids, avs) bs = do
  counters <- currentCounters
  mkAnsBox (c:cs, ids, avs) [chC counters, exerC counters]
     (putStrTR "答案")
     (renderBlocks bs)

renderDIV c@"center" (cs, ids, avs) bs = do
  mkTagAttrsC "center" (cs, ids, avs)
    (renderBlocks bs)

renderDIV c@"proof" (cs, ids, avs) bs =
  mkTagAttrsC "div" (c:cs, ids, avs)
    (do mkTag "b" (putStrTR "證明 ")
        renderBlocks bs)

renderDIV c (cls, ids, avs) bs =
  mkTagAttrsC "div" (c:cls, ids, avs)
    (renderBlocks bs)

renderHeader :: Int -> [Attr] -> Inlines -> RMonad ()
renderHeader hd attrs is = do
  (_, nums) <- state (newHeader hd)
  let (htag, hcls) = seclevel hd
  mkTagAttrs htag (AtrClass hcls : attrs)
      (do printSecNum nums
          renderInlines is)
 where seclevel 1 = ("h1", "chapter")
       seclevel 2 = ("h2", "section")
       seclevel 3 = ("h3", "subsection")
       seclevel 4 = ("h4", "subsubsection")

printSecNum :: [Int] -> RMonad ()
printSecNum []     = return ()
printSecNum [i]    = putStrR (show i) >> putCharR ' '
printSecNum (i:is) = putStrR (show i) >> putCharR '.' >> printSecNum is


renderInlines :: Inlines -> RMonad ()
renderInlines = mapM_ renderInline

renderInline :: Inline -> RMonad ()
renderInline (Str txt) = putStrTR txt
renderInline Space     = putCharR ' '
renderInline SoftBreak = putStrTR "&shy;"
renderInline LineBreak = putStrTR "<br/>\n"
renderInline (Emph inlines) =
  mkTag "em" (renderInlines inlines)
renderInline (Strong inlines) =
  mkTag "strong" (renderInlines inlines)
renderInline (Code txt) =
  mkTag "code" (putStrTR txt)
renderInline (HsCode txt) =
  mkTagAttrsC "code" (["haskell"], [], []) (putStrTR txt)
renderInline (Tex txt) = putCharR '$' >> putStrTR txt >> putCharR '$'
renderInline (Entity txt) = putStrTR txt -- not sure what to do yet
renderInline (RawHtml txt) = putStrTR txt
renderInline (Attrs attrs) = return () -- deal with this later
renderInline (Footnote is) = do
     (_, (i:_)) <- state newFNote
     mkTagAttrsC "span" (["footnote"], [], [])
      (do putStrTR "註" >> putStrR (show i) >> putStrTR ": "
          renderInlines is)
renderInline (Ref txt)   = renderRef txt
renderInline (EqRef txt) = putCharR '(' >> renderRef txt >> putCharR ')'
renderInline (PageRef txt) = return () -- deal with this later
renderInline (Index idx) = return () -- deal with this later
renderInline (CiteT ref Nothing) = return () -- deal with this later
renderInline (CiteT ref (Just opt)) = return () -- deal with this later
renderInline (CiteP [(ref, Just opt)]) = return () -- deal with this later
renderInline (CiteP cites) = return () -- deal with this later

renderRef :: Text -> RMonad ()
renderRef lbl = do
    res <- lookupLbl lbl
    case res of
      Nothing -> putStrTR "[RefUndefined]"
      Just (file, nums) -> do
        href <- showHRef file lbl
        mkTagAttrsC "a" ([], [], [("href", href)])
                    (printSecNum nums)

showHRef :: [Int] -> Text -> RMonad Text
showHRef (ch:_) lbl = do
   b <- isThisFile ch
   if b then return (Text.cons '#' lbl)
      else do fname <- chToFileName ch
              return (Text.append (Text.pack (fname ++ ".html#")) lbl)


renderCode :: ([Text], [Text], [(Text, Text)]) -> Text -> RMonad ()
renderCode (cs,ids,avs) txt | "invisible" `elem` cs = return ()
renderCode (cs,ids,avs) txt | "texonly" `elem` cs = return ()

renderCode (cs,ids,avs) txt | "equation" `elem` cs =
  mkTag "pre" (do
    (_, nums) <- state newEq
    mkTagAttrsC "code" (cs, ids, avs) (putStrTR txt)
    putStrTR "    ("
    printSecNum nums
    putStrTR ")\n")

renderCode (cs,ids,avs) txt =
  mkTag "pre" (
    mkTagAttrsC "code" (cs, ids, avs)
      (putStrTR txt >> putCharR '\n'))

--- TOC

renderTOCsList :: TOC -> RMonad ()
renderTOCsList [] = return ()
renderTOCsList ts =
  mkTag "ul" (mapM_ renderTOCList ts)

renderTOCList :: Rose TOCItem -> RMonad ()
renderTOCList (RNode tocitem ts) = do
  renderTOCItem tocitem
  renderTOCsList ts

renderTOCItem :: TOCItem -> RMonad ()
renderTOCItem ((fid, nums), title, lbl) = do
  href <- showHRef fid lbl
  mkTag "li"
   (mkTagAttrsC "a" ([],[],[("href", href)])
     (do printSecNum nums
         renderInlines title))

renderTOCPartial :: TOC -> RMonad PRTOC
renderTOCPartial = mapM renderTOCPartialItem
  where renderTOCPartialItem (RNode ((fid, nums), title, lbl) ts) =
          do href <- showHRef fid lbl
             ts' <- renderTOCPartial ts
             return $
               RNode ((href, nums),
                      (printSecNum nums >> renderInlines title),
                      lbl)
                     ts'

--

sepChHeader :: Blocks -> (Maybe Block, Blocks)
sepChHeader Empty = (Nothing, Empty)
sepChHeader (Header 1 attrs is :<| blocks) =
  (Just (Header 1 attrs is), blocks)
sepChHeader (Header hd attrs is :<| blocks) =
  (Nothing, Header hd attrs is :<| blocks)
sepChHeader (bl :<| blocks) =
  (id *** (bl :<|)) $ sepChHeader blocks
