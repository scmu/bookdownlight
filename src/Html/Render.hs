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
renderBlock (Para (Attrs attrs :<| is)) =
  mkTagAttrs "p" attrs' (do
    case lookupAttrs "title" attrs of
        Just title -> mkTagAttrs "b" attrs' (putStrTR title) >> putStrTR " &emsp;"
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
renderBlock (CodeBlock attrs txt) = renderCode cls ids avs txt
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs
renderBlock (DIV attrs bs)
     | []     <- cls = renderBlocks bs
     | (c:cs) <- cls = renderDIV c cs ids avs bs
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

renderDIV :: Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> RMonad ()
renderDIV _ _ _ _ _ = return () 
{-
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

renderLabel h xs = hPutStr h " id='" >> T.hPutStr h xs >> hPutStr h "'"
--renderLabel h xs = T.hPutStr h xs >> T.hPutStr h "'>" >> T.hPutStr h "1.1"
renderLabel' h xs = do
  T.hPutStr h xs
  T.hPutStr h "'>"

-}

renderHeader :: Int -> [Attr] -> Inlines -> RMonad ()
renderHeader hd attrs is =
  let (htag, hcls) = seclevel hd
  in mkTagAttrs htag (AtrClass hcls : attrs)
        (renderInlines is)
 where seclevel 1 = ("h1", "chapter")
       seclevel 2 = ("h2", "section")
       seclevel 3 = ("h3", "subsection")
       seclevel 4 = ("h4", "subsubsection")

renderInlines :: Inlines -> RMonad ()
renderInlines _ = return ()

renderCode :: [Text] -> [Text] -> [(Text, Text)] -> Text -> RMonad ()
renderCode _ _ _ _ = return ()

renderAttrsC :: AttrsC -> RMonad ()
renderAttrsC (cls, ids, avs) =
   renderCls cls >> renderIds ids >> renderAVs avs
 where  -- classes joined together, separated by space
       renderCls [] = return ()
       renderCls (c:cs) = putStrTR " class=\"" >> putStrTR c >>
                          renderCls0 cs
       renderCls0 [] = putCharR '"'
       renderCls0 (c:cs) = putCharR ' ' >> putStrTR c >> renderCls0 cs
        -- there can be at most one id
       renderIds [] = return ()
       renderIds (i:_) = putStrTR " id=\"" >> putStrTR i >> putCharR '"'

renderAVs :: [(Text, Text)] -> RMonad ()
renderAVs = mapM_ renderAttr
  where renderAttr (a, v) = do
           putCharR ' ' >> putStrTR a >> putStrTR "=\""
           putStrTR v >> putCharR '"'

mkTag tag body = mkTagAttrsC tag ([], [], []) body

mkTagAttrs :: Text -> [Attr] -> RMonad () -> RMonad ()
mkTagAttrs tag attrs body =
  mkTagAttrsC tag (sortAttrs attrs) body

mkTagAttrsC :: Text -> AttrsC -> RMonad () -> RMonad ()
mkTagAttrsC tag attrs body = do
   putCharR '<' >> putStrTR tag >> renderAttrsC attrs >> putCharR '>'
   body
   putStrR "</" >> putStrTR tag >> putCharR '>'
