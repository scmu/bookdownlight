{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Html.HtmlRender where

import System.IO (hPutChar, hPutStr, Handle, hPrint)
import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Cheapskate
import Syntax.Util
import Data.List (intersperse, find, intercalate)
import Html.Dict

{-
type DictState = [([Int], Text)]
addDict :: Int -> String -> State DictState String
addDict id label = do
  dict <- get
  let ids = case dict of
                     []                      -> [1, 0, 0, 0]
                     (([a, b, c, d], _) : _) -> case id of
                                                    --0 -> dictState
                                                1 -> [a + 1, 0, 0, 0]
                                                2 -> [a, b + 1, 0, 0]
                                                3 -> [a, b, c + 1, 0]
                                                4 -> [a, b, c, d + 1]
  put ((ids, label) : dict)
  return (concat . intersperse "." . map show $ ids)


getDict :: State DictState String
getDict = do
  (nums, _) <- gets Prelude.head
  return (concat . intersperse "." . map show $ nums)
-}

htmlRender :: Handle -> Doc -> IO ()
htmlRender h (Doc _ blocks) = renderBlocks h blocks

renderBlocks :: Handle -> Blocks -> IO ()
renderBlocks h = mapM_ (renderBlock h)

renderBlock :: Handle -> Block -> IO ()
renderBlock h (Para (Attrs attrs :<| is)) = do
  hdParaHeader h attrs
  renderInlines h is
  hPutStr h "\n"
renderBlock h (Para is) = do
  hPutStr h "<p>"
  renderInlines h is
  hPutStr h "</p>\n"
renderBlock h (Header hd attrs is) = renderHeader h hd attrs is
renderBlock h (Blockquote bs) =
  do hPutStr h "<blockquote>\n"
     renderBlocks h bs
     hPutStr h "</blockquote>\n"
renderBlock h (List _ lt items) =
  do hPutStr h ("<" ++ ltype  )
     mapM_ renderLItem items
     hPutStr h ("</"++ ltype )
 where ltype = case lt of
         Bullet _     -> "ul>\n"
         Numbered _ _ -> "ol>\n"
       renderLItem bs = do
         hPutStr h "<li>\n"
         renderBlocks h bs
         hPutStr h "</li>\n"
renderBlock h (CodeBlock attrs txt) = renderCode h cls ids avs txt
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

renderBlock h (DIV attrs bs)
     | [] <- cls    = renderBlocks h bs
     | (c:cs) <- cls = renderDIV h c cs ids avs bs
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

-- renderBlock (DIV attrs bs) = dbegin >> renderBlocks bs >> dend
--   where (dbegin, dend) = renderDIVBrackets attrs

renderDIV :: Handle -> Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> IO ()
renderDIV h c cs ids avs bs | c `elem` thmEnvs = do
  envBegin h c ids
  hPutStr h "<b>" >> T.hPutStr h env
  if not $ null ids then do latexCmd h c $ Prelude.head ids else do hPutStr h " 0.0"
  case lookup "title" avs of
    Nothing -> return ()
    Just title -> hPutStr h "<b>" >> T.hPutStr h title >> hPutStr h "</b>"
  --mapM_ (renderLabel h) ids
  hPutStr h "</b><br>\n"
  renderBlocks h bs
  envEnd h "</div>"
  where thmEnvs :: [Text]
        thmEnvs = ["theorem", "lemma", "definition", "example", "corollary"]
        env = case c of
              "theorem" -> "定理"
              "lemma" -> "引理"
              "definition" -> "定義"
              "example" -> "範例"
              "corollary" -> "推論"

renderDIV h "figure" cs ids avs bs = do
  hPutStr h "<div id='"
  mapM_ (renderLabel h) ids
  hPutStr h ">\n<img src=''"
  printPositions cs
  hPutStr h ">\n"
  renderBlocks h bs
  printCaption avs
  hPutStr h "</div>\n"
  where printPositions cs
          | [] <- ps = return ()
          | otherwise = hPutChar h '[' >> mapM_ (hPutChar h . Data.Text.head) ps >>
                       hPutChar h ']'
        ps = filter (`elem` poses) cs
          where poses = ["here", "top", "bottom", "page"]
        printCaption avs =
          case lookup "title" avs of
            Just cap -> do
              hPutStr h "<figcaption>圖"
              if not $ null ids then do latexCmd h "figure" $ Prelude.head ids else do hPutStr h " 0.0"
              T.hPutStr h cap
              hPutStr h "</figcaption>\n"
            Nothing -> return ()

renderDIV h "texonly" _ _ _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = T.hPutStr h code >> hPutStr h "<br>\n"
        renderTexOnly b = renderBlock h b

renderDIV h "infobox" _ _ avs bs = do
   T.hPutStr h "<div class = 'infobox'>\n"
   printTitle avs
   hPutStr h "\n"
   renderBlocks h (fmap infoindent bs)
   T.hPutStr h "</div>"
 where printTitle avs = case lookup "title" avs of
         Just cap -> T.hPutStr h "<b>" >> T.hPutStr h cap >> T.hPutStr h "</b><br>\n"
         Nothing -> return ()
       infoindent (Para is) = Para (Str " " :<| is )
       infoindent b = b

renderDIV h "multicols" _ _ avs bs = do
  T.hPutStr h "<table>\n"
  renderBlocks h bs
  T.hPutStr h "</table>\n"

renderDIV h "mcol" _ _ avs bs = do
  case lookup "width" avs of
    Just w -> do T.hPutStr h "<tr>\n"
                 --T.hPutStr h w
                 T.hPutStr h "<td>\n"
                 renderBlocks h bs
                 T.hPutStr h "</td>\n</tr>\n"
    Nothing -> renderBlocks h bs

renderDIV h "exlist" _ _ _ bs = do
  hPutStr h "<div class='exlist'>\n"
  hPutStr h "\n"
  renderBlocks h bs
  T.hPutStr h "</div>\n"

renderDIV h "exer" _ ids _ bs = do
  hPutStr h "<div class='exercise'"
  mapM_ (renderLabel h) ids
  hPutStr h ">\n<b>練習"
  if not $ null ids then do latexCmd h "exer" $ Prelude.head ids else do hPutStr h " 0.0"
  hPutStr h "</b><br>\n"
  renderBlocks h bs
  hPutStr h "</div>\n"

renderDIV h "exans" cs _ _ bs = do
  hPutStr h "<div class='answer'>\n<b>答案</b>"
  printCompact
  hPutStr h "\n"
  renderBlocks h bs
  hPutStr h "</div>\n"
 where printCompact | "compact" `elem` cs = T.hPutStr h " "
                    | otherwise = return ()

renderDIV h "answer" _ ids _ bs = do
  hPutStr h "<div class='answer'"
  mapM_ (renderLabel h) ids
  hPutStr h ">\n<b>答案</b><br>\n"
  renderBlocks h bs
  hPutStr h "</div>\n"

renderDIV h "center" cs ids avs bs = do
  hPutStr h "<center>"
  renderBlocks h bs
  envEnd h "</center>"

renderDIV h "proof" _ ids _ bs = do
  hPutStr h "<div class='proof'"
  mapM_ (renderLabel h) ids
  hPutStr h ">\n<b>證明</b><br>\n"
  renderBlocks h bs
  hPutStr h "</div>\n"

 -- catch-all case.
 -- possible instances: example, answer.
renderDIV h c cs ids avs bs = do
  envBegin h c ids
  T.hPutStr h c >> hPutChar h ':'
  renderBlocks h bs
  envEnd h "</div>"

renderHeader :: Handle -> Int -> [Attr] -> Inlines -> IO ()
renderHeader h hd attrs is = do
  hPutStr h (seclevel hd)
  if null (attrsId attrs)
    then do hPutStr h "'>"
    else do
      hPutStr h hasId
      mapM_ (renderLabel' h) (attrsId attrs)
      latexCmd h "header" $ Prelude.head ids
  renderInlines h is
  hPutStr h (seclevel_end hd)
     --addDict hd "label"
  where seclevel 1 = "<h1 class='chapter"
        seclevel 2 = "<h2 class='section"
        seclevel 3 = "<h3 class='subsection"
        seclevel 4 = "<h4 class='subsubsection"
        hasId = "' id='"
        seclevel_end 1 = "</h1>\n"
        seclevel_end 2 = "</h2>\n"
        seclevel_end 3 = "</h3>\n"
        seclevel_end 4 = "</h4>\n"
        ids = attrsId attrs

renderLabel h xs = hPutStr h " id='" >> T.hPutStr h xs >> hPutStr h "'"
--renderLabel h xs = T.hPutStr h xs >> T.hPutStr h "'>" >> T.hPutStr h "1.1"
renderLabel' h xs = do
  T.hPutStr h xs
  T.hPutStr h "'>"



renderCode :: Handle -> [Text] -> [Text] -> [(Text, Text)] -> Text -> IO ()
renderCode h cls ids _ txt | "spec" `elem` cls =
  do envBegin h "<pre class='spec'" ids
     --mapM_ (renderLabel h) ids
     --hPutStr h "\n"
     T.hPutStr h txt
     --hPutStr h "\n"
     envEnd h "</pre><br>"
renderCode h ("haskell" : cs) ids _ txt =
  do when invisible (T.hPutStr h "\n")
     envBegin h "<pre style='display:none'" ids
     --mapM_ (renderLabel h) ids
     hPutStr h "\n"
     T.hPutStr h txt
     hPutStr h "\n<br>"
     envEnd h "</pre><br>"
     when invisible (T.hPutStr h " ")
 where invisible = "invisible" `elem` cs
renderCode h ("texonly" : _) ids _ txt = do
  envBegin h "<pre class='texonly'" ids
  T.hPutStr h txt
  envEnd h "</pre><br>"
renderCode h ("verbatim" : cs) ids _ txt  =
  do envBegin h "<pre class='verbatim'" ids
     hPutStr h "\n"
     T.hPutStr h txt
     hPutStr h "\n"
     envEnd h "</pre><br>"

renderCode h ("equation" : cs) ids _ txt  =
  do envBegin h "<code class='haskell'" ids
     hPutStr h "\n"
     T.hPutStr h txt
     envEnd h "</code> （式"
     if not $ null ids then do latexCmd h "eq" $ Prelude.head ids else do hPutStr h " 0.0"
     hPutStr h "）<br>"

renderCode h _ ids _ txt = do
  do envBegin h "<code class='haskell'" ids
     --mapM_ (renderLabel h) ids
     hPutStr h "\n"
     T.hPutStr h txt
     hPutStr h "\n"
     envEnd h "</code>"

envBegin :: Handle -> Text -> [Text] -> IO ()
envBegin h env ids = do
  if env `elem` thmEnvs
    then hPutStr h "<div class='" >> T.hPutStr h env >> hPutStr h "'"
    else T.hPutStr h env
  mapM_ (renderLabel h) ids >> hPutStr h ">\n"
  where thmEnvs = ["theorem", "lemma", "definition", "example", "answer", "proof", "corollary"]


envEnd :: Handle -> Text -> IO ()
--envEnd h env = hPutStr h "" >> T.hPutStr h env >> hPutStr h "</div>\n"
envEnd h env = T.hPutStr h env >> hPutStr h "\n"

hdParaHeader :: Handle -> [Attr] -> IO()
hdParaHeader h attrs = do
  case lookupAttrs "title" attrs of
    Just title -> hPutStr h "<br>\n<b class='title'" >> mapM_ (renderLabel h) (attrsId attrs) >> hPutStr h ">" >> T.hPutStr h title >> hPutStr h "</b> &emsp;"
    _ | hasClass "noindent" attrs -> T.hPutStr h "" >> mapM_ (renderLabel h) (attrsId attrs)
    _ | hasClass "nobreak"  attrs -> mapM_ (renderLabel h) (attrsId attrs)
    _ -> mapM_ (renderLabel h) (attrsId attrs)

{-

data Block = Para Inlines
           | Header Int Inlines
           | Blockquote Blocks
           | List Bool ListType [Blocks]
           | CodeBlock CodeAttr Text
           | HtmlBlock Text
           | HRule
           deriving (Show, Data, Typeable)

-}

renderInlines :: Handle -> Inlines -> IO ()
renderInlines h = mapM_ (renderInline h)

renderInline :: Handle -> Inline -> IO ()
renderInline h (Str txt) = T.hPutStr h txt
renderInline h Space = hPutStr h " "
renderInline h SoftBreak = hPutStr h ""
renderInline h LineBreak = hPutStr h "<br>27\n"
renderInline h (Emph inlines) =
  do hPutStr h "<em> "
     renderInlines h inlines
     hPutStr h " </em>"
renderInline h (Strong inlines) =
  do hPutStr h "<b>"
     renderInlines h inlines
     hPutStr h "</b>"
renderInline h (Code txt) = hPutStr h "<code>" >> T.hPutStr h txt >> hPutStr h "</code>"
renderInline h (HsCode txt) = hPutStr h "<code class='haskell'>" >> T.hPutStr h txt >> hPutStr h "</code>"
renderInline h (Tex txt) = hPutChar h '$' >> T.hPutStr h txt >> hPutChar h '$'
renderInline h (Entity txt) = T.hPutStr h txt >> T.hPutStr h " Entity "
renderInline h (RawHtml txt) = T.hPutStr h txt
renderInline h (Attrs attrs) = mapM_ (renderLabel h) (attrsId attrs)
renderInline h (Footnote is) =
  do hPutStr h "\n<p class='footnote'>\n註："
     renderInlines h is
     hPutStr h "</p>\n"
renderInline h (Ref txt)   = latexCmd h "ref" txt
renderInline h (EqRef txt) = hPutStr h "式" >> latexCmd h "eqref" txt
renderInline h (PageRef txt) = latexCmd h "pageref" txt
renderInline h (Index idx) = latexCmd h "index" idx
renderInline h (CiteT ref Nothing) = latexCmd h "citet1" ref
renderInline h (CiteT ref (Just opt)) = latexCmdOpt h "citet2" opt ref
renderInline h (CiteP [(ref, Just opt)]) = latexCmdOpt h "citep" opt ref
renderInline h (CiteP cites) = -- with multiple citation we ignore options.
  do hPutStr h "\\citep{"
     putRefs cites
     hPutChar h '}'
 where putRefs [] = return ()
       putRefs [(ref,_)] = T.hPutStr h ref
       putRefs ((ref,_):cites) = T.hPutStr h ref >> hPutChar h ',' >>
                                 putRefs cites

latexCmd :: Handle -> Text -> Text -> IO ()
latexCmd h cmd arg = if cmd == "index" then return () else do
  --hPutChar h ' '
  when (cmd `elem` citeList) $ T.hPutStr h cmd
  hPutChar h ' '
  --T.hPutStr h arg
  let output = find (\(l, v, _, _) -> l == arg) dict
  case output of
    Nothing -> do hPutStr h "MISSED"
    Just (l, v, _, _) -> do
      hPutStr h "<a href='"
      T.hPutStr h root
      hPutStr h ".html#"
      T.hPutStr h l
      hPutStr h "'>"
      hPutStr h $ intercalate "." . map show $ v
      hPutStr h " </a>"
      where root = case Prelude.head v of
              1 -> "Introduction"
              2 -> "Basics"
              3 -> "Induction"
              4 -> "Semantics"
              5 -> "Derivation"
              6 -> "Folds"
              7 -> "SegProblems"
  where citeList = ["citet1", "citet2", "citep"]


latexCmdOpt :: Handle -> Text -> Text -> Text -> IO ()
latexCmdOpt h cmd opt arg =
  hPutChar h '\\' >> T.hPutStr h cmd >>
  hPutChar h '[' >> T.hPutStr h opt >> hPutStr h "]" >>
  hPutStr h "<b>" >> T.hPutStr h arg >> hPutStr h "</b>"
