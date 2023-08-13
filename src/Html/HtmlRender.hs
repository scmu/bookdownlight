{-# LANGUAGE OverloadedStrings #-}
module Html.HtmlRender where

import System.IO (hPutChar, hPutStr, Handle)
import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Cheapskate
import Syntax.Util
import Data.List (intersperse)

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
  hPutStr h "<br>\n"
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
   envBegin h c
   case lookup "title" avs of
     Nothing -> return ()
     Just title -> hPutChar h '[' >> T.hPutStr h title >> hPutChar h ']'
   mapM_ (renderLabel h) ids
   hPutStr h "<br>\n"
   renderBlocks h bs
   envEnd h c
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example"]

renderDIV h "figure" cs ids avs bs = do
   T.hPutStr h "<img src='"
   printPositions cs
   hPutStr h "<br>5\n"
   renderBlocks h bs
   printCaption avs
   mapM_ (renderLabel h) ids
   hPutStr h "<br>6\n"
   T.hPutStr h "'><br>7\n"
 where printPositions cs
         | [] <- ps = return ()
         | otherwise = hPutChar h '[' >> mapM_ (hPutChar h . Data.Text.head) ps >>
                       hPutChar h ']'
        where poses = ["here", "top", "bottom", "page"]
              ps = filter (\c -> c `elem` poses) cs
       printCaption avs =
         case lookup "title" avs of
           Just cap -> T.hPutStr h "\\caption{" >> T.hPutStr h cap >> T.hPutStr h "}<br>8\n"
           Nothing -> return ()

renderDIV h "texonly" _ _ _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = T.hPutStr h code >> hPutStr h "\n"
        renderTexOnly b = renderBlock h b

renderDIV h "infobox" _ _ avs bs = do
   T.hPutStr h "<div class = 'infobox'>\n"
   printTitle avs
   hPutStr h "\n"
   renderBlocks h (fmap infoindent bs)
   T.hPutStr h "</div>"
 where printTitle avs = case lookup "title" avs of
         Just cap -> T.hPutStr h "{" >> T.hPutStr h cap >> T.hPutStr h "}<br>11\n"
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
                 T.hPutStr h w
                 T.hPutStr h "<td>\n"
                 renderBlocks h bs
                 T.hPutStr h "</td>\n</tr>\n"
    Nothing -> renderBlocks h bs

renderDIV h "exlist" _ _ _ bs = do
  T.hPutStr h "<div class = 'exlist'>\n"
  hPutStr h "\n"
  renderBlocks h bs
  T.hPutStr h "</div>\n"

renderDIV h "exer" _ ids _ bs = do
  T.hPutStr h "<div class = 'Exercise'>\n<b>練習:</b>"
  mapM_ (renderLabel h) ids
  hPutStr h "\n"
  renderBlocks h bs
  hPutStr h "</div>\n"

renderDIV h "exans" cs _ _ bs = do
  T.hPutStr h "<div class = 'Answer'>\n<b>答:</b>"
  printCompact
  hPutStr h "\n"
  renderBlocks h bs
  hPutStr h "</div>\n"
 where printCompact | "compact" `elem` cs = T.hPutStr h " "
                    | otherwise = return ()

 -- catch-all case.
 -- possible instances: example, answer.
renderDIV h c cs ids avs bs = do
  envBegin h c >> mapM_ (renderLabel h) ids >> hPutStr h "\n"
  renderBlocks h bs
  envEnd h c

renderHeader :: Handle -> Int -> [Attr] -> Inlines -> IO ()
renderHeader h hd attrs is =
  do hPutStr h (seclevel hd)
     mapM_ (renderLabel' h) (attrsId attrs)
     renderInlines h is
     hPutStr h (seclevel_end hd)
     --addDict hd "label"

 where seclevel 1 = "<h1 class='chapter' id='"
       seclevel 2 = "<h2 class='section' id='"
       seclevel 3 = "<h3 class='subsection' id='"
       seclevel 4 = "<h4 class='subsubsection' id='"
       seclevel_end 1 = "</h1>"
       seclevel_end 2 = "</h2>"
       seclevel_end 3 = "</h3>"
       seclevel_end 4 = "</h4>"

renderLabel h xs = T.hPutStr h "<id='" >> T.hPutStr h xs >> T.hPutStr h "'>"
--renderLabel h xs = T.hPutStr h xs >> T.hPutStr h "'>" >> T.hPutStr h "1.1"
renderLabel' h xs = do
     T.hPutStr h xs
     T.hPutStr h "'>"

renderCode :: Handle -> [Text] -> [Text] -> [(Text, Text)] -> Text -> IO ()
renderCode h cls ids _ txt | "spec" `elem` cls =
  do envBegin h "<pre class='spec'>"
     mapM_ (renderLabel h) ids
     hPutStr h "\n"
     T.hPutStr h txt
     hPutStr h "\n"
     envEnd h "</pre>"
renderCode h ("haskell" : cs) ids _ txt  =
  do when invisible (T.hPutStr h "\n")
     envBegin h "<pre style='display:none'>"
     mapM_ (renderLabel h) ids
     hPutStr h "\n"
     T.hPutStr h txt
     hPutStr h "\n"
     envEnd h "</pre>"
     when invisible (T.hPutStr h " ")
 where invisible = "invisible" `elem` cs
renderCode h ("texonly" : _) _ _ txt =
  T.hPutStr h txt >> hPutStr h "<br>22\n"
renderCode h ("verbatim" : cs) ids _ txt  =
  do envBegin h "verbatim"
     hPutStr h "<br>23\n"
     T.hPutStr h txt
     hPutStr h "<br>\n"
     envEnd h "verbatim"
renderCode h _ ids _ txt = do
  do envBegin h "<code class='haskell'>"
     mapM_ (renderLabel h) ids
     hPutStr h "\n"
     T.hPutStr h txt
     hPutStr h "\n"
     envEnd h "</code>"

envBegin :: Handle -> Text -> IO ()
envBegin h env = hPutStr h "<div>" >> T.hPutStr h env >> hPutStr h ""

envEnd :: Handle -> Text -> IO ()
envEnd h env = hPutStr h "" >> T.hPutStr h env >> hPutStr h "</div>\n"

hdParaHeader :: Handle -> [Attr] -> IO()
hdParaHeader h attrs = do
  case lookupAttrs "title" attrs of
    Just title -> T.hPutStr h ("") >> T.hPutStr h title >> hPutStr h " "
    _ | hasClass "noindent" attrs -> T.hPutStr h ""
    _ | hasClass "nobreak"  attrs -> return ()
    _ -> return ()
  mapM_ (renderLabel h) (attrsId attrs)

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
  do hPutStr h "<em>"
     renderInlines h inlines
     hPutStr h "</em>"
renderInline h (Strong inlines) =
  do hPutStr h "<b>"
     renderInlines h inlines
     hPutStr h "</b>"
renderInline h (Code txt) = hPutStr h "<code>" >> T.hPutStr h txt >> hPutStr h "<code>"
renderInline h (HsCode txt) = hPutStr h "<code class='haskell'>" >> T.hPutStr h txt >> hPutStr h "</code>"
renderInline h (Tex txt) = hPutChar h '$' >> T.hPutStr h txt >> hPutChar h '$'
renderInline h (Entity txt) = T.hPutStr h txt
renderInline h (RawHtml txt) = T.hPutStr h txt
renderInline h (Attrs attrs) = mapM_ (renderLabel h) (attrsId attrs)
renderInline h (Footnote is) =
  do hPutStr h "<p class='footer'>"
     renderInlines h is
     hPutStr h "</p>"
renderInline h (Ref txt)   = latexCmd h "ref" txt
renderInline h (EqRef txt) = latexCmd h "eqref" txt
renderInline h (PageRef txt) = latexCmd h "pageref" txt
renderInline h (Index idx) = latexCmd h "index" idx
renderInline h (CiteT ref Nothing) = latexCmd h "citet" ref
renderInline h (CiteT ref (Just opt)) = latexCmdOpt h "citet" opt ref
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
latexCmd h cmd arg =
  hPutChar h ' ' >> T.hPutStr h cmd >>
  hPutChar h ' ' >> T.hPutStr h arg >> hPutChar h ' '

latexCmdOpt :: Handle -> Text -> Text -> Text -> IO ()
latexCmdOpt h cmd opt arg =
  hPutChar h '\\' >> T.hPutStr h cmd >>
  hPutChar h '[' >> T.hPutStr h opt >> hPutStr h "]" >>
  hPutStr h "<b>" >> T.hPutStr h arg >> hPutStr h "</b>"
