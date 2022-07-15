{-# LANGUAGE OverloadedStrings #-}
module Html.HtmlRender where

import System.IO (hPutChar, hPutStr, Handle)
import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Cheapskate

htmlRender :: Handle -> Doc -> IO ()
htmlRender h (Doc _ blocks) = renderBlocks h blocks

renderBlocks :: Handle -> Blocks -> IO ()
renderBlocks h = mapM_ (renderBlock h)

renderBlock :: Handle -> Block -> IO ()
renderBlock h (Para (Attrs attrs :<| is)) = do
  hdParaHeader h attrs
  renderInlines h is
  hPutStr h "<br1>\n"
renderBlock h (Para is) = do
  hPutStr h "<p>"
  renderInlines h is
  hPutStr h "</p>\n"
renderBlock h (Header hd attrs is) = renderHeader h hd attrs is
renderBlock h (Blockquote bs) =
  do hPutStr h "\\blockquote{<br>\n"
     renderBlocks h bs
     hPutStr h "}<br>\n"
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
   hPutStr h "<br4>\n"
   renderBlocks h bs
   envEnd h c
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example"]

renderDIV h "figure" cs ids avs bs = do
   T.hPutStr h "<img src='"
   printPositions cs
   hPutStr h "<b5r>\n"
   renderBlocks h bs
   printCaption avs
   mapM_ (renderLabel h) ids
   hPutStr h "<br6>\n"
   T.hPutStr h "'><br7>\n"
 where printPositions cs
         | [] <- ps = return ()
         | otherwise = hPutChar h '[' >> mapM_ (hPutChar h . Data.Text.head) ps >>
                       hPutChar h ']'
        where poses = ["here", "top", "bottom", "page"]
              ps = filter (\c -> c `elem` poses) cs
       printCaption avs =
         case lookup "title" avs of
           Just cap -> T.hPutStr h "\\caption{" >> T.hPutStr h cap >> T.hPutStr h "}<br8>\n"
           Nothing -> return ()

renderDIV h "texonly" _ _ _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = T.hPutStr h code >> hPutStr h "\n"
        renderTexOnly b = renderBlock h b

renderDIV h "infobox" _ _ avs bs = do
   T.hPutStr h "\\begin{infobox}"
   printTitle avs
   hPutStr h "\n"
   renderBlocks h (fmap infoindent bs)
   T.hPutStr h "\\end{infobox}"
 where printTitle avs = case lookup "title" avs of
         Just cap -> T.hPutStr h "{" >> T.hPutStr h cap >> T.hPutStr h "}<br11>\n"
         Nothing -> return ()
       infoindent (Para is) = Para (Str "\\quad " :<| is )
       infoindent b = b

renderDIV h "multicols" _ _ avs bs = do
  T.hPutStr h "\\\\<br12>\n"
  renderBlocks h bs
  T.hPutStr h "\\\\"

renderDIV h "mcol" _ _ avs bs = do
  case lookup "width" avs of
    Just w -> do T.hPutStr h "\\begin{minipage}{"
                 T.hPutStr h w
                 T.hPutStr h "}<br13>\n"
                 renderBlocks h bs
                 T.hPutStr h "\\end{minipage}<br>\n"
    Nothing -> renderBlocks h bs

renderDIV h "exlist" _ _ _ bs = do
  T.hPutStr h "\\begin{exlist}"
  hPutStr h "<br14>\n"
  renderBlocks h bs
  T.hPutStr h "\\end{exlist}"

renderDIV h "exer" _ ids _ bs = do
  T.hPutStr h "\\Exercise"
  mapM_ (renderLabel h) ids
  hPutStr h "<br15>\n"
  renderBlocks h bs

renderDIV h "exans" cs _ _ bs = do
  T.hPutStr h "\\Answer"
  printCompact
  hPutStr h "<br16>\n"
  renderBlocks h bs
 where printCompact | "compact" `elem` cs = T.hPutStr h "~\\\\ \\vspace{-0.5cm}"
                    | otherwise = return ()

 -- catch-all case.
 -- possible instances: example, answer.
renderDIV h c cs ids avs bs = do
  envBegin h c >> mapM_ (renderLabel h) ids >> hPutStr h "<br17>"
  renderBlocks h bs
  envEnd h c

renderHeader :: Handle -> Int -> [Attr] -> Inlines -> IO ()
renderHeader h hd attrs is =
  do hPutStr h (seclevel hd)
     renderInlines h is
     mapM_ (renderLabel h) (attrsId attrs)
     hPutStr h (seclevel_end hd)
 where seclevel 1 = "<h1 class='chapter'>"
       seclevel 2 = "<h2 class='section' >"
       seclevel 3 = "<h3 class='subsection' >"
       seclevel 4 = "<h4 class='subsubsection' >"
       seclevel_end 1 = "</h1>"
       seclevel_end 2 = "</h2>"
       seclevel_end 3 = "</h3>"
       seclevel_end 4 = "</h4>"

renderLabel h xs = T.hPutStr h "\\label{" >> T.hPutStr h xs >> hPutChar h '}'

renderCode :: Handle -> [Text] -> [Text] -> [(Text, Text)] -> Text -> IO ()
renderCode h cls ids _ txt | "spec" `elem` cls =
  do envBegin h "<div class='spec'>"
     mapM_ (renderLabel h) ids
     hPutStr h "<br20>\n"
     T.hPutStr h txt
     hPutStr h "<br>\n"
     envEnd h "</div>"
renderCode h ("haskell" : cs) ids _ txt  =
  do when invisible (T.hPutStr h "%if False<br>\n")
     envBegin h "code"
     mapM_ (renderLabel h) ids
     hPutStr h "<br21>\n"
     T.hPutStr h txt
     hPutStr h "<br>\n"
     envEnd h "code"
     when invisible (T.hPutStr h "%endif")
 where invisible = "invisible" `elem` cs
renderCode h ("texonly" : _) _ _ txt =
  T.hPutStr h txt >> hPutStr h "<br22>\n"
renderCode h ("verbatim" : cs) ids _ txt  =
  do envBegin h "verbatim"
     hPutStr h "<br23>\n"
     T.hPutStr h txt
     hPutStr h "<br>\n"
     envEnd h "verbatim"
renderCode h _ ids _ txt = do
  do envBegin h "code"
     mapM_ (renderLabel h) ids
     hPutStr h "<br24>\n"
     T.hPutStr h txt
     hPutStr h "<br>\n"
     envEnd h "code"

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
renderInline h LineBreak = hPutStr h "<br27>\n"
renderInline h (Emph inlines) =
  do hPutStr h "<em>"
     renderInlines h inlines
     hPutStr h "</em>"
renderInline h (Strong inlines) =
  do hPutStr h "<b>"
     renderInlines h inlines
     hPutStr h "</b>"
renderInline h (Code txt) = hPutChar h '`' >> T.hPutStr h txt >> hPutChar h '`'
renderInline h (HsCode txt) = hPutStr h "<code>" >> T.hPutStr h txt >> hPutStr h "</code>"
renderInline h (Tex txt) = hPutChar h '$' >> T.hPutStr h txt >> hPutChar h '$'
renderInline h (Entity txt) = T.hPutStr h txt
renderInline h (RawHtml txt) = T.hPutStr h txt
renderInline h (Attrs attrs) = mapM_ (renderLabel h) (attrsId attrs)
renderInline h (Footnote is) =
  do hPutStr h "\\footnote{"
     renderInlines h is
     hPutChar h '}'
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
  hPutChar h '\\' >> T.hPutStr h cmd >>
  hPutChar h '{' >> T.hPutStr h arg >> hPutChar h '}'

latexCmdOpt :: Handle -> Text -> Text -> Text -> IO ()
latexCmdOpt h cmd opt arg =
  hPutChar h '\\' >> T.hPutStr h cmd >>
  hPutChar h '[' >> T.hPutStr h opt >> hPutChar h ']' >>
  hPutChar h '{' >> T.hPutStr h arg >> hPutChar h '}'
