{-# LANGUAGE OverloadedStrings #-}
module Html.Render where

import System.IO (Handle)
import qualified System.IO as IO (hPutChar, hPutStr)
import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T (hPutStr)
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Reader

import Cheapskate

import Syntax.Util

import Html.Counter

type RMonad = ReaderT (Handle, LblMap) (StateT Counter IO)

putStrR   xs = ReaderT (liftIO . flip IO.hPutStr xs . fst)
putCharR  c  = ReaderT (liftIO . flip IO.hPutChar c . fst)
putStrTR  xs = ReaderT (liftIO . flip T.hPutStr  xs . fst)

htmlRender :: Doc -> RMonad ()
htmlRender (Doc _ blocks) = renderBlocks blocks

renderBlocks :: Blocks -> RMonad ()
renderBlocks = mapM_ renderBlock

renderBlock :: Block -> RMonad ()
-- renderBlock = undefined
renderBlock (Para (Attrs attrs :<| is)) = do
  hdParaHeader attrs
  renderInlines is
  putCharR \n'
renderBlock (Para is) = do
  putStrTR "<p>"
  renderInlines is
  putStrTR "</p>\n"
renderBlock (Header hd attrs is) = renderHeader hd attrs is
renderBlock (Blockquote bs) =
  do putStrTR "<blockquote>\n"
     renderBlocks bs
     putStrTR "</blockquote>\n"
renderBlock (List _ lt items) =
  do putStrTR ("<" ++ ltype  )
     mapM_ renderLItem items
     putStrTR ("</"++ ltype )
 where ltype = case lt of
         Bullet _     -> "ul>\n"
         Numbered _ _ -> "ol>\n"
       renderLItem bs = do
         putStrTR "<li>\n"
         renderBlocks bs
         putStrTR "</li>\n"
renderBlock (CodeBlock attrs txt) = renderCode cls ids avs txt
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

renderBlock (DIV attrs bs)
     | [] <- cls     = renderBlocks bs
     | (c:cs) <- cls = renderDIV c cs ids avs bs
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

renderDIV :: Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> RMonad ()
renderDIV c cs ids avs bs | c `elem` thmEnvs = do
  envBegin c ids
  putStrTR "<b>" >> putStrTR env
  if not (null ids) then latexCmd c $ Prelude.head ids
                    else putStrTR " 0.0"
  case lookup "title" avs of
    Nothing -> return ()
    Just title -> putStrTR "<b>" >> putStrTR title >> putStrTR "</b>"
  --mapM_ (renderLabel h) ids
  putStrTR "</b><br>\n"
  renderBlocks bs
  envEnd "</div>"
  where thmEnvs :: [Text]
        thmEnvs = ["theorem", "lemma", "definition", "example", "corollary"]
        env = case c of
              "theorem" -> "定理"
              "lemma" -> "引理"
              "definition" -> "定義"
              "example" -> "範例"
              "corollary" -> "推論"

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

renderDIV "texonly" _ _ _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = putStrTR code >> putStrTR "<br>\n"
        renderTexOnly b = renderBlock h b

renderDIV "infobox" _ _ avs bs = do
   putStrTR "<div class = 'infobox'>\n"
   printTitle avs
   putStrTR "\n"
   renderBlocks (fmap infoindent bs)
   putStrTR "</div>"
 where printTitle avs = case lookup "title" avs of
         Just cap -> putStrTR "<b>" >> putStrTR cap >> putStrTR "</b><br>\n"
         Nothing -> return ()
       infoindent (Para is) = Para (Str " " :<| is )
       infoindent b = b

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

renderDIV "exlist" _ _ _ bs = do
  putStrTR "<div class='exlist'>\n"
  putStrTR "\n"
  renderBlocks bs
  putStrTR "</div>\n"

renderDIV "exer" _ ids _ bs = do
  putStrTR "<div class='exercise'"
  mapM_ (renderLabel h) ids
  putStrTR ">\n<b>練習"
  if not $ null ids then do latexCmd h "exer" $ Prelude.head ids else do putStrTR " 0.0"
  putStrTR "</b><br>\n"
  renderBlocks bs
  putStrTR "</div>\n"

renderDIV "exans" cs _ _ bs = do
  putStrTR "<div class='answer'>\n<b>答案</b>"
  printCompact
  putStrTR "\n"
  renderBlocks bs
  putStrTR "</div>\n"
 where printCompact | "compact" `elem` cs = putStrTR " "
                    | otherwise = return ()

renderDIV "answer" _ ids _ bs = do
  putStrTR "<div class='answer'"
  mapM_ (renderLabel h) ids
  putStrTR ">\n<b>答案</b><br>\n"
  renderBlocks bs
  putStrTR "</div>\n"

renderDIV "center" cs ids avs bs = do
  putStrTR "<center>"
  renderBlocks bs
  envEnd h "</center>"

renderDIV "proof" _ ids _ bs = do
  putStrTR "<div class='proof'"
  mapM_ (renderLabel h) ids
  putStrTR ">\n<b>證明</b><br>\n"
  renderBlocks bs
  putStrTR "</div>\n"

 -- catch-all case.
 -- possible instances: example, answer.
renderDIV c cs ids avs bs = do
  envBegin c ids
  putStrTR c >> hPutChar h ':'
  renderBlocks bs
  envEnd h "</div>"
