{-# LANGUAGE OverloadedStrings #-}
module Book.TexRender where

import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Cheapskate

texRender :: Doc -> IO ()
texRender (Doc _ blocks) = renderBlocks blocks

renderBlocks :: Blocks -> IO ()
renderBlocks = mapM_ renderBlock

renderBlock :: Block -> IO ()
renderBlock (Para (Attrs attrs :<| is)) = do
  hdParaHeader attrs
  renderInlines is
  putStr "\n\n"
renderBlock (Para is) = do
  renderInlines is
  putStr "\n\n"
renderBlock (Header h attrs is) = renderHeader h attrs is
renderBlock (Blockquote bs) =
  do putStr "\\blockquote{\n"
     renderBlocks bs
     putStr "}\n"
renderBlock (List _ lt items) =
  do putStr ("\\begin{" ++ ltype  ++ "}\n")
     mapM_ renderLItem items
     putStr ("\\end{" ++ ltype ++ "}\n")
 where ltype = case lt of
         Bullet _     -> "itemize"
         Numbered _ _ -> "enum"
       renderLItem bs = do
         putStr "\\item "
         renderBlocks bs
         -- putStr "\n"
renderBlock (CodeBlock attrs cs) = renderCode attrs cs
renderBlock (DIV attrs bs)
     | [] <- cls    = renderBlocks bs
     | (c:cs) <- cls = renderDIV c cs ids avs bs
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

-- renderBlock (DIV attrs bs) = dbegin >> renderBlocks bs >> dend
--   where (dbegin, dend) = renderDIVBrackets attrs

renderDIV :: Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> IO ()
renderDIV c cs ids avs bs | c `elem` thmEnvs = do
   envBegin c
   case lookup "title" avs of
     Nothing -> return ()
     Just title -> putChar '[' >> T.putStr title >> putChar ']'
   mapM_ renderLabel ids
   putChar '\n'
   renderBlocks bs
   envEnd c
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example"]

renderDIV "figure" cs ids avs bs = do
   T.putStr "\\begin{figure}"
   printPositions cs
   putChar '\n'
   renderBlocks bs
   printCaption avs
   T.putStr "\\end{figure}"
 where printPositions cs
         | [] <- ps = return ()
         | otherwise = putChar '[' >> mapM_ (putChar . Data.Text.head) ps >>
                       putChar ']'
        where poses = ["here", "top", "bottom", "page"]
              ps = filter (\c -> c `elem` poses) cs
       printCaption avs =
         case lookup "title" avs of
           Just cap -> T.putStr "\\caption{" >> T.putStr cap >> T.putStr "}\n"
           Nothing -> return ()

renderDIV c cs ids avs bs = do
  envBegin c >> mapM_ renderLabel ids >> putChar '\n'
  renderBlocks bs
  envEnd c

-- renderDIVBrackets :: [Attr] -> (IO (), IO ())
-- renderDIVBrackets attrs = (dbeg cls, dend cls)
--  where ids = attrsId attrs
--        cls = attrsClass attrs
--        avs = attrsAVs attrs
--
--        dbeg [] = return ()
--        dbeg (c:_)
--          | c `elem` thmEnvs = do
--              envBegin c
--              case lookup "title" avs of
--                Nothing -> return ()
--                Just title -> putChar '[' >> T.putStr title >> putChar ']'
--              mapM_ renderLabel ids
--              putChar '\n'
--          | otherwise = envBegin c >> mapM_ renderLabel ids >> putChar '\n'
--
--        dend []    = return ()
--        dend (c:_) = envEnd c
--
--        thmEnvs :: [Text]
--        thmEnvs = ["theorem", "lemma", "definition", "example"]

renderHeader h attrs is =
  do putStr ('\\': seclevel h ++ "{")
     renderInlines is
     putChar '}'
     mapM_ renderLabel (attrsId attrs)
     putStr "\n\n"
 where seclevel 1 = "chapter"
       seclevel 2 = "section"
       seclevel 3 = "subsection"
       seclevel 4 = "subsubsection"

renderLabel xs = T.putStr "\\label{" >> T.putStr xs >> putChar '}'

renderCode attrs cs =
  do envBegin codetype
     putChar '\n'
     T.putStr cs
     putChar '\n'
     envEnd codetype
 where codetype :: Text
       codetype = if AtrClass "spec" `elem` attrs
                   then "spec" else "code"

envBegin :: Text -> IO ()
envBegin env = putStr "\\begin{" >> T.putStr env >> putStr "}"

envEnd :: Text -> IO ()
envEnd env = putStr "\\end{" >> T.putStr env >> putStr "}\n"

hdParaHeader :: [Attr] -> IO()
hdParaHeader attrs = do
  when (hasClass "noindent" attrs) (T.putStr "\\noindent ")
  case lookupAttrs "title" attrs of
    Just title -> T.putStr ("\\paragraph{") >> T.putStr title >> putChar '}'
    Nothing -> return ()
  mapM_ renderLabel (attrsId attrs)

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

renderInlines :: Inlines -> IO ()
renderInlines = mapM_ renderInline

renderInline :: Inline -> IO ()
renderInline (Str txt) = T.putStr txt
renderInline Space = putStr " "
renderInline SoftBreak = putStr "\n"
renderInline LineBreak = putStr "\n"
renderInline (Emph inlines) =
  do putStr "\\emph{"
     renderInlines inlines
     putStr "}"
renderInline (Strong inlines) =
  do putStr "{\\bf "
     renderInlines inlines
     putStr "}"
renderInline (Code txt) = putChar '`' >> T.putStr txt >> putChar '`'
renderInline (HsCode txt) = putChar '|' >> T.putStr txt >> putChar '|'
renderInline (Entity txt) = T.putStr txt
renderInline (RawHtml txt) = T.putStr txt
renderInline (Attrs attrs) = mapM_ renderLabel (attrsId attrs)
renderInline (Footnote is) =
  do putStr "\\footnote{"
     renderInlines is
     putChar '}'
renderInline (Ref txt)   = latexCmd "ref" txt
renderInline (EqRef txt) = latexCmd "eqref" txt
renderInline (Index idx) = latexCmd "index" idx
renderInline (CiteT ref Nothing) = latexCmd "citet" ref
renderInline (CiteT ref (Just opt)) = latexCmdOpt "citet" opt ref
renderInline (CiteP [(ref, Just opt)]) = latexCmdOpt "citep" opt ref
renderInline (CiteP cites) = -- with multiple citation we ignore options.
  do putStr "\\citep{"
     putRefs cites
     putChar '}'
 where putRefs [] = return ()
       putRefs [(ref,_)] = T.putStr ref
       putRefs ((ref,_):cites) = T.putStr ref >> putChar ',' >>
                                 putRefs cites

latexCmd :: Text -> Text -> IO ()
latexCmd cmd arg =
  putChar '\\' >> T.putStr cmd >>
  putChar '{' >> T.putStr arg >> putChar '}'

latexCmdOpt :: Text -> Text -> Text -> IO ()
latexCmdOpt cmd opt arg =
  putChar '\\' >> T.putStr cmd >>
  putChar '[' >> T.putStr opt >> putChar ']' >>
  putChar '{' >> T.putStr arg >> putChar '}'

{-

data Inline = Str Text
            | Space
            | SoftBreak
            | LineBreak
            | Emph Inlines
            | Strong Inlines
            | Code Text
            | Link Inlines Text {- URL -} Text {- title -}
            | Image Inlines Text {- URL -} Text {- title -}
            | Entity Text
            | RawHtml Text
            | Attrs [Attr]
            | Footnote Inlines
            | Index Text
            | Ref Text
            | CiteP [(Text, Maybe Text)]  -- (citation, options)
            | CiteT Text (Maybe Text)     -- Name (year, options)
            deriving (Show, Data, Typeable)

-}
