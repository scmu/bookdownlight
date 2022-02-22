module Book.TexRender where

import Data.Sequence (Seq)
import qualified Data.Text.IO as T
import Cheapskate

texRender :: Doc -> IO ()
texRender (Doc _ blocks) = mapM_ renderBlock blocks

renderBlock :: Block -> IO ()
renderBlock (Para inlines) = do
  renderInlines inlines
  putStr "\n\n"
renderBlock (Header h _ inlines) =
  do putStr "\\section{"
     renderInlines inlines
     putStr "}\n"

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
render (Entity txt) = T.putStr txt
render (RawHtml txt) = T.putStr txt

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
            deriving (Show, Data, Typeable)

-}
