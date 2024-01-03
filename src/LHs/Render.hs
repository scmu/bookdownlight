{-# LANGUAGE OverloadedStrings #-}
module LHs.Render (lhsRender, LHsMonad(..)) where

import System.IO (Handle)
import qualified System.IO as IO (hPutChar, hPutStr)
import Data.Maybe (isJust)
import Data.Sequence (Seq(..))
import Data.Foldable (toList)
import Data.Text (Text, head)
import Data.List (partition)
import qualified Data.Text.IO as T (hPutStr)
import Control.Monad (when, (<=<))
import Control.Monad.Reader
import Control.Arrow ((***))
import Cheapskate
import Syntax.Util

type LHsMonad = ReaderT Handle IO

putStrR   xs = ReaderT (flip IO.hPutStr xs)
putCharR  c  = ReaderT (flip IO.hPutChar c)
putStrTR  xs = ReaderT (flip T.hPutStr  xs)

---

lhsRender :: Doc -> LHsMonad ()
lhsRender (Doc _ blocks) = renderBlocks blocks

renderBlocks :: Blocks -> LHsMonad ()
renderBlocks = mapM_ renderBlock

renderBlock :: Block -> LHsMonad ()
renderBlock (Para (Attrs attrs :<| is)) = do
  hdParaHeader attrs
  renderInlines is
  putCharR '\n'
renderBlock (Para is) = do
  putCharR '\n'
  renderInlines is
  putCharR '\n'
renderBlock (Header hd attrs is) = renderHeader hd attrs is
renderBlock (Blockquote bs) =
  mkEnv' "quote" (putStrR "{\\em")
      (do renderBlocks bs
          putStrR "}%\\em\n")
renderBlock (List _ lt items) =
  mkEnv ltype (mapM_ renderLItem items)
 where ltype = case lt of
         Bullet _     -> "compactitem"
         Numbered _ _ -> "compactenum"
       renderLItem bs = do
         putStrR "\\item "
         renderBlocks bs
         -- putStr "\n"
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

-- renderBlock (DIV attrs bs) = dbegin >> renderBlocks bs >> dend
--   where (dbegin, dend) = renderDIVBrackets attrs

renderDIV :: Text -> [Text] -> [Text]
          -> [(Text, Text)] -> Blocks -> LHsMonad ()
renderDIV c cs ids avs bs | c `elem` thmEnvs =
   mkEnv' c
    (do case lookup "title" avs of
          Nothing -> return ()
          Just title -> putCharR '[' >> putStrTR title >> putCharR ']'
        mapM_ renderLabel ids)
    (renderBlocks bs >> putCharR '\n')
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example",
                  "proof", "corollary"]

renderDIV "figure" cs ids avs bs =
   mkEnv' "figure" (printPositions cs)
    (do renderBlocks bs
        printCaption avs
        mapM_ renderLabel ids
        putCharR '\n')
 where printPositions cs
         | [] <- ps = return ()
         | otherwise = putCharR '[' >> mapM_ (putCharR . Data.Text.head) ps >>
                       putCharR ']'
        where poses = ["here", "top", "bottom", "page"]
              ps = filter (\c -> c `elem` poses) cs
       printCaption avs =
         case lookup "title" avs of
           Just cap -> putStrTR "\\caption{" >> putStrTR cap >> putStrTR "}\n"
           Nothing -> return ()

renderDIV "texonly" _ _ _ bs = mapM_ renderTexOnly bs
  where renderTexOnly (CodeBlock _ code) = putStrTR code >> putCharR '\n'
        renderTexOnly b = renderBlock b

renderDIV "infobox" _ _ avs bs = do
   putStrTR "\\begin{infobox}"
   printTitle avs
   putCharR '\n'
   renderBlocks (fmap infoindent bs)
   putStrTR "\\end{infobox}"
 where printTitle avs = case lookup "title" avs of
         Just cap -> putStrTR "{" >> putStrTR cap >> putStrTR "}\n"
         Nothing -> return ()
       infoindent (Para is) = Para (Str "\\quad " :<| is )
       infoindent b = b

renderDIV "multicols" _ _ avs bs = do
  putStrTR "\\\\\n"
  renderBlocks bs
  putStrTR "\\\\"

renderDIV "mcol" _ _ avs bs = do
  case lookup "width" avs of
    Just w -> do putStrTR "\\begin{minipage}{"
                 putStrTR w
                 putStrTR "}\n"
                 renderBlocks bs
                 putStrTR "\\end{minipage}\n"
    Nothing -> renderBlocks bs

renderDIV "exlist" _ _ _ bs = do
  putStrTR "\\begin{exlist}"
  putCharR '\n'
  renderBlocks bs
  putStrTR "\\end{exlist}"

renderDIV "exer" _ ids _ bs = do
  putStrTR "\\Exercise"
  mapM_ renderLabel ids
  putCharR '\n'
  renderBlocks bs

renderDIV "exans" cs _ _ bs = do
  putStrTR "\\Answer"
  printCompact
  putCharR '\n'
  renderBlocks bs
 where printCompact | "compact" `elem` cs = putStrTR "~\\\\ \\vspace{-0.5cm}"
                    | otherwise = return ()

renderDIV (c@"equations") cs ids avs bs = do
   case parseEquations bs of
     Just eqs -> renderEquations eqs
     Nothing ->  -- falling back to catch-all case
       mkEnv' c (mapM_ renderLabel ids)
        (renderBlocks bs)

 -- catch-all case.
 -- possible instances: example, answer.
renderDIV c cs ids avs bs =
  mkEnv' c (mapM_ renderLabel ids)
   (renderBlocks bs)

renderHeader :: Int -> [Attr] -> Inlines -> LHsMonad ()
renderHeader hd attrs is =
  do putCharR '\n'
     putStrR ('\\': seclevel hd ++ "{")
     renderInlines is
     putCharR '}'
     mapM_ renderLabel (attrsId attrs)
     putCharR '\n'
 where seclevel 1 = "chapter"
       seclevel 2 = "section"
       seclevel 3 = "subsection"
       seclevel 4 = "subsubsection"

renderLabel xs = putStrTR "\\label{" >> putStrTR xs >> putCharR '}'

renderCode :: [Text] -> [Text] -> [(Text, Text)] -> Text -> LHsMonad ()
renderCode cls ids _ txt | "spec" `elem` cls =
  do when (not (null ids))
        (mapM_ renderLabel ids >> putCharR '\n')
     mkEnv "spec" (putStrTR txt >> putCharR '\n')

renderCode cls ids _ txt | "haskell" `elem` cls =
  do when invisible (putStrTR "%if False\n")
     when (not (null ids))
      (mapM_ renderLabel ids >> putCharR '\n')
     mkEnv "code" (putStrTR txt >> putCharR '\n')
     when invisible (putStrTR "%endif\n")
 where invisible = "invisible" `elem` cls
renderCode cls ids avs txt | "equation" `elem` cls =
     mkEnv alignEnv
      (do case lookup "title" avs of
            Nothing  -> return ()
            Just ttl -> putStrTR "\\textbf{" >>
                        putStrTR ttl >> putStrTR "}~ "
          putStrTR txt
          mapM_ renderLabel ids
          putCharR '\n')
 where alignEnv | null ids = "align*"
                | otherwise = "align"
renderCode cls _ _ txt | "texonly" `elem` cls =
  putStrTR txt >> putCharR '\n'
renderCode ("verbatim" : cs) ids _ txt  =
  mkEnv "verbatim" (putStrTR txt >> putCharR '\n')
renderCode _ ids _ txt = do
  do when (not (null ids))
       (mapM_ renderLabel ids >> putCharR '\n')
     mkEnv "code"  (putStrTR txt >> putCharR '\n')

renderEquations :: [(Maybe Text, Maybe Text, [Text])] -> LHsMonad ()
                   -- (title, id, formulae)
renderEquations eqs = do
    mkEnv "align" (renderEquations eqs)
  where hasTitle  = any (isJust . fst3) eqs
        allSingle = all (isSingle . trd3) eqs
        fst3 (x,_,_) = x
        trd3 (_,_,z) = z
        isSingle [_] = True
        isSingle _   = False

        renderEquations :: [(Maybe Text, Maybe Text, [Text])] -> LHsMonad ()
        renderEquations [] = return ()
        renderEquations [eq] = renderEquation eq >> putCharR '\n'
        renderEquations (eq:eqs) = do
           renderEquation eq
           putStrTR "\\\\\n"
           renderEquations eqs

        renderEquation :: (Maybe Text, Maybe Text, [Text]) -> LHsMonad ()
        renderEquation (title, iid, fms) = do
          when hasTitle (do
            case title of Nothing  -> putStrTR " & & "
                          Just ttl -> do putStrTR " & \\textbf{"
                                         putStrTR ttl
                                         putStrTR "} & "
            )
          when allSingle (putStrTR " & ")
          renderFs fms
          case iid of Nothing -> putStrTR " \\notag "
                      Just lbl -> do putStrTR "\\label{"
                                     putStrTR lbl
                                     putStrTR "}"

        renderFs []     = return ()
        renderFs [f]    = putCharR '|' >> putStrTR f >> putCharR '|'
        renderFs (f:fs) = do putCharR '|'
                             putStrTR f
                             putStrTR "| & "
                             renderFs fs

mkEnv' :: Text -> LHsMonad () -> LHsMonad () -> LHsMonad ()
mkEnv' env pre body = do
  putStrTR "\\begin{" >> putStrTR env >> putCharR '}'
  pre  -- things before the first newline
  putCharR '\n'
  body
  putStrTR "\\end{" >> putStrTR env >> putStrR "}\n"
  -- \begin{env}pre
  --   body\end{env}
  -- let body generate the last newline

mkEnv :: Text -> LHsMonad () -> LHsMonad ()
mkEnv env body = mkEnv' env (return ()) body
   -- newline immediately after \begin{env}

hdParaHeader :: [Attr] -> LHsMonad ()
hdParaHeader attrs = do
  case lookupAttrs "title" attrs of
    Just title -> putStrTR ("\n\\paragraph{") >> putStrTR title >> putCharR '}'
    _ | hasClass "noindent" attrs -> putStrTR "\n\\noindent "
    _ | hasClass "nobreak"  attrs -> putStrTR "\n\\noindent " -- return ()
    _ -> return ()
  mapM_ renderLabel (attrsId attrs)

renderInlines :: Inlines -> LHsMonad ()
renderInlines = mapM_ renderInline

renderInline :: Inline -> LHsMonad ()
renderInline (Str txt) = putStrTR txt
renderInline Space = putStrR " "
renderInline SoftBreak = putCharR '\n'
renderInline LineBreak = putCharR '\n'
renderInline (Emph inlines) =
  do putStrR "\\emph{"
     renderInlines inlines
     putStrR "}"
renderInline (Strong inlines) =
  do putStrR "{\\bf "
     renderInlines inlines
     putStrR "}"
renderInline (Code txt) = putCharR '`' >> putStrTR txt >> putCharR '`'
renderInline (HsCode txt) = putCharR '|' >> putStrTR txt >> putCharR '|'
renderInline (Tex txt) = putCharR '$' >> putStrTR txt >> putCharR '$'
renderInline (Entity txt) = putStrTR txt
renderInline (RawHtml txt) = putStrTR txt
renderInline (Attrs attrs) = mapM_ renderLabel (attrsId attrs)
renderInline (Footnote is) =
  do putStrR "\\footnote{"
     renderInlines is
     putCharR '}'
renderInline (Ref txt)   = latexCmd "ref" txt
renderInline (EqRef txt) = latexCmd "eqref" txt
renderInline (PageRef txt) = latexCmd "pageref" txt
renderInline (Index idx) = latexCmd "index" idx
renderInline (CiteT ref Nothing) = latexCmd "citet" ref
renderInline (CiteT ref (Just opt)) = latexCmdOpt "citet" opt ref
renderInline (CiteP [(ref, Just opt)]) = latexCmdOpt "citep" opt ref
renderInline (CiteP cites) = -- with multiple citation we ignore options.
  do putStrR "\\citep{"
     putRefs cites
     putCharR '}'
 where putRefs [] = return ()
       putRefs [(ref,_)] = putStrTR ref
       putRefs ((ref,_):cites) = putStrTR ref >> putCharR ',' >>
                                 putRefs cites

latexCmd :: Text -> Text -> LHsMonad ()
latexCmd cmd arg =
  putCharR '\\' >> putStrTR cmd >>
  putCharR '{' >> putStrTR arg >> putCharR '}'

latexCmdOpt ::  Text -> Text -> Text -> LHsMonad ()
latexCmdOpt cmd opt arg =
  putCharR '\\' >> putStrTR cmd >>
  putCharR '[' >> putStrTR opt >> putCharR ']' >>
  putCharR '{' >> putStrTR arg >> putCharR '}'

--
parseEquations :: Blocks -> Maybe [(Maybe Text, Maybe Text, [Text])]
                            -- (title, id, formulae)
parseEquations bs = concat <$> mapM pEqList bs
 where pEqList :: Block -> Maybe [(Maybe Text, Maybe Text, [Text])]
       pEqList (List _ _ eqs) = mapM (pEq <=< concatPara) eqs
       pEqList _ = Nothing

       concatPara :: Blocks -> Maybe [Inline]
       concatPara Empty = return []
       concatPara (Para ins :<| ps) = (toList ins ++) <$> concatPara ps
       concatPara _ = Nothing

       pEq :: [Inline] -> Maybe (Maybe Text, Maybe Text, [Text])
       pEq xs = Just (lookupAttrs "title" attrs,
                      head' . attrsId $ attrs,
                      hss)
          where (attrs, hss) =
                   ((concat . map unAttrs) ***
                    map unHsCode . filter isHsCode) .
                    partition isAttrs $ xs
                head' []     = Nothing
                head' (x:xs) = Just x
