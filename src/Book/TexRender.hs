{-# LANGUAGE OverloadedStrings #-}
module Book.TexRender where

import System.IO (hPutChar, hPutStr, Handle)
import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Cheapskate

texRender :: Handle -> Doc -> IO ()
texRender h (Doc _ blocks) = renderBlocks h blocks

renderBlocks :: Handle -> Blocks -> IO ()
renderBlocks h = mapM_ (renderBlock h)

renderBlock :: Handle -> Block -> IO ()
renderBlock h (Para (Attrs attrs :<| is)) = do
  hdParaHeader h attrs
  renderInlines h is
  hPutStr h "\n"
renderBlock h (Para is) = do
  hPutStr h "\n"
  renderInlines h is
  hPutStr h "\n"
renderBlock h (Header hd attrs is) = renderHeader h hd attrs is
renderBlock h (Blockquote bs) =
  do envBegin h "quote"
     hPutStr h "{\\em"
     renderBlocks h bs
     hPutStr h "}%\\em\n"
     envEnd h "quote"
renderBlock h (List _ lt items) =
  do hPutStr h ("\\begin{" ++ ltype  ++ "}\n")
     mapM_ renderLItem items
     hPutStr h ("\\end{" ++ ltype ++ "}\n")
 where ltype = case lt of
         Bullet _     -> "compactitem"
         Numbered _ _ -> "compactenum"
       renderLItem bs = do
         hPutStr h "\\item "
         renderBlocks h bs
         -- putStr "\n"
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
   hPutChar h '\n'
   renderBlocks h bs
   envEnd h c
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example",
                  "proof"]

renderDIV h "figure" cs ids avs bs = do
   T.hPutStr h "\\begin{figure}"
   printPositions cs
   hPutChar h '\n'
   renderBlocks h bs
   printCaption avs
   mapM_ (renderLabel h) ids
   hPutChar h '\n'
   T.hPutStr h "\\end{figure}\n"
 where printPositions cs
         | [] <- ps = return ()
         | otherwise = hPutChar h '[' >> mapM_ (hPutChar h . Data.Text.head) ps >>
                       hPutChar h ']'
        where poses = ["here", "top", "bottom", "page"]
              ps = filter (\c -> c `elem` poses) cs
       printCaption avs =
         case lookup "title" avs of
           Just cap -> T.hPutStr h "\\caption{" >> T.hPutStr h cap >> T.hPutStr h "}\n"
           Nothing -> return ()

renderDIV h "texonly" _ _ _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = T.hPutStr h code >> hPutChar h '\n'
        renderTexOnly b = renderBlock h b

renderDIV h "infobox" _ _ avs bs = do
   T.hPutStr h "\\begin{infobox}"
   printTitle avs
   hPutChar h '\n'
   renderBlocks h (fmap infoindent bs)
   T.hPutStr h "\\end{infobox}"
 where printTitle avs = case lookup "title" avs of
         Just cap -> T.hPutStr h "{" >> T.hPutStr h cap >> T.hPutStr h "}\n"
         Nothing -> return ()
       infoindent (Para is) = Para (Str "\\quad " :<| is )
       infoindent b = b

renderDIV h "multicols" _ _ avs bs = do
  T.hPutStr h "\\\\\n"
  renderBlocks h bs
  T.hPutStr h "\\\\"

renderDIV h "mcol" _ _ avs bs = do
  case lookup "width" avs of
    Just w -> do T.hPutStr h "\\begin{minipage}{"
                 T.hPutStr h w
                 T.hPutStr h "}\n"
                 renderBlocks h bs
                 T.hPutStr h "\\end{minipage}\n"
    Nothing -> renderBlocks h bs

renderDIV h "exlist" _ _ _ bs = do
  T.hPutStr h "\\begin{exlist}"
  hPutChar h '\n'
  renderBlocks h bs
  T.hPutStr h "\\end{exlist}"

renderDIV h "exer" _ ids _ bs = do
  T.hPutStr h "\\Exercise"
  mapM_ (renderLabel h) ids
  hPutChar h '\n'
  renderBlocks h bs

renderDIV h "exans" cs _ _ bs = do
  T.hPutStr h "\\Answer"
  printCompact
  hPutChar h '\n'
  renderBlocks h bs
 where printCompact | "compact" `elem` cs = T.hPutStr h "~\\\\ \\vspace{-0.5cm}"
                    | otherwise = return ()

 -- catch-all case.
 -- possible instances: example, answer.
renderDIV h c cs ids avs bs = do
  envBegin h c >> mapM_ (renderLabel h) ids >> hPutChar h '\n'
  renderBlocks h bs
  envEnd h c

renderHeader :: Handle -> Int -> [Attr] -> Inlines -> IO ()
renderHeader h hd attrs is =
  do hPutChar h '\n'
     hPutStr h ('\\': seclevel hd ++ "{")
     renderInlines h is
     hPutChar h '}'
     mapM_ (renderLabel h) (attrsId attrs)
     hPutChar h '\n'
 where seclevel 1 = "chapter"
       seclevel 2 = "section"
       seclevel 3 = "subsection"
       seclevel 4 = "subsubsection"

renderLabel h xs = T.hPutStr h "\\label{" >> T.hPutStr h xs >> hPutChar h '}'

renderCode :: Handle -> [Text] -> [Text] -> [(Text, Text)] -> Text -> IO ()
renderCode h cls ids _ txt | "spec" `elem` cls =
  do when (not (null ids))
        (mapM_ (renderLabel h) ids >> hPutChar h '\n')
     envBegin h "spec"
     hPutChar h '\n'
     T.hPutStr h txt
     hPutChar h '\n'
     envEnd h "spec"
renderCode h cls ids _ txt | "haskell" `elem` cls =
  do when invisible (T.hPutStr h "%if False\n")
     when (not (null ids))
      (mapM_ (renderLabel h) ids >> hPutChar h '\n')
     envBegin h "code"
     hPutChar h '\n'
     T.hPutStr h txt
     hPutChar h '\n'
     envEnd h "code"
     when invisible (T.hPutStr h "%endif\n")
 where invisible = "invisible" `elem` cls
renderCode h cls ids _ txt | "equation" `elem` cls =
  do envBegin h alignEnv
     hPutChar h '\n'
     T.hPutStr h txt
     mapM_ (renderLabel h) ids
     hPutChar h '\n'
     envEnd h alignEnv
 where alignEnv | null ids = "align*"
                | otherwise = "align"
renderCode h cls _ _ txt | "texonly" `elem` cls =
  T.hPutStr h txt >> hPutChar h '\n'
renderCode h ("verbatim" : cs) ids _ txt  =
  do envBegin h "verbatim"
     hPutChar h '\n'
     T.hPutStr h txt
     hPutChar h '\n'
     envEnd h "verbatim"
renderCode h _ ids _ txt = do
  do when (not (null ids))
       (mapM_ (renderLabel h) ids >> hPutChar h '\n')
     envBegin h "code"
     hPutChar h '\n'
     T.hPutStr h txt
     hPutChar h '\n'
     envEnd h "code"

envBegin :: Handle -> Text -> IO ()
envBegin h env = hPutStr h "\\begin{" >> T.hPutStr h env >> hPutStr h "}"

envEnd :: Handle -> Text -> IO ()
envEnd h env = hPutStr h "\\end{" >> T.hPutStr h env >> hPutStr h "}\n"

hdParaHeader :: Handle -> [Attr] -> IO()
hdParaHeader h attrs = do
  case lookupAttrs "title" attrs of
    Just title -> T.hPutStr h ("\n\\paragraph{") >> T.hPutStr h title >> hPutChar h '}'
    _ | hasClass "noindent" attrs -> T.hPutStr h "\n\\noindent "
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
renderInline h SoftBreak = hPutStr h "\n"
renderInline h LineBreak = hPutStr h "\n"
renderInline h (Emph inlines) =
  do hPutStr h "\\emph{"
     renderInlines h inlines
     hPutStr h "}"
renderInline h (Strong inlines) =
  do hPutStr h "{\\bf "
     renderInlines h inlines
     hPutStr h "}"
renderInline h (Code txt) = hPutChar h '`' >> T.hPutStr h txt >> hPutChar h '`'
renderInline h (HsCode txt) = hPutChar h '|' >> T.hPutStr h txt >> hPutChar h '|'
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
