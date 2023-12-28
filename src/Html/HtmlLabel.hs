{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Html.HtmlLabel where

import System.IO (hPutChar, hPutStr, Handle, hPrint)
import Data.Sequence (Seq(..), index, drop)
import Data.Text (Text, head, unpack)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Cheapskate
import Syntax.Util
import Data.List (intersperse)
import System.Console.GetOpt (ArgDescr(NoArg))


type SecCount = [Int]
type ThmCount = [Int]
type ContCount = [Int]
type DictState = (Text, [Int], Text, [Int]) -- (label, value, type, root) should be (key, value)


updateSecCount :: Int -> StateT ([Int], [Int], [Int]) IO ([Int], [Int]) --
updateSecCount hd = do
  count <- get
  let ([a, b, c, d], thmcnt, contcnt) = count
  let (counter, ids) = case hd of
                          1 -> ([a, 0, 0, 0], [a])
                          2 -> ([a, b+1, 0, 0], [a, b+1])
                          3 -> ([a, b, c+1, 0], [a, b, c+1])
                          4 -> ([a, b, c, d+1], [a, b, c, d+1])
  put (counter, thmcnt, contcnt)
  return (ids, counter)

updateThmCount :: Int -> Int -> StateT ([Int], [Int], [Int]) IO ([Int], [Int])
updateThmCount t ch = do
  count <- get
  let (seccnt, [th, lm, df, ep, ec, eq, cd, fg, co], contcnt) = count
  let (counter, ids) = case t of
                        1 -> ([th+1, lm, df, ep, ec, eq, cd, fg, co], [ch, th+1]) -- theorem
                        2 -> ([th, lm+1, df, ep, ec, eq, cd, fg, co], [ch, lm+1]) -- lemma
                        3 -> ([th, lm, df+1, ep, ec, eq, cd, fg, co], [ch, df+1]) -- definition
                        4 -> ([th, lm, df, ep+1, ec, eq, cd, fg, co], [ch, ep+1]) -- example
                        5 -> ([th, lm, df, ep, ec+1, eq, cd, fg, co], [ch, ec+1]) -- exercise
                        6 -> ([th, lm, df, ep, ec, eq+1, cd, fg, co], [ch, eq+1]) -- equation
                        7 -> ([th, lm, df, ep, ec, eq, cd+1, fg, co], [ch, cd+1]) -- code
                        8 -> ([th, lm, df, ep, ec, eq, cd, fg+1, co], [ch, fg+1]) -- figure
                        9 -> ([th, lm, df, ep, ec, eq, cd, fg, co+1], [ch, co+1]) -- corollary
  put (seccnt, counter, contcnt)
  return (ids, counter)

updateContCount :: Int -> Int -> StateT ([Int], [Int], [Int]) IO ([Int], [Int])
updateContCount t ch = do
  count <- get
  let (seccnt, thmcnt, [fn, attr, idx]) = count
  let (counter, ids) = case t of
                  1 -> ([fn+1, attr, idx], [ch, fn+1])
                  2 -> ([fn, attr+1, idx], [ch, attr+1])
                  3 -> ([fn, attr, idx+1], [ch, idx+1])
  put (seccnt, thmcnt, counter)
  return (ids, counter)

increment :: Text -> StateT ([Int], [Int], [Int]) IO ([Int], [Int])
increment t = do
  counter <- get
  let (seccnt, thmcnt, fncnt) = counter
  let ch = Prelude.head seccnt
  (ids, root) <- case t of
    "chapter" -> updateSecCount 1
    "section" -> updateSecCount 2
    "subsection" -> updateSecCount 3
    "subsubsection" -> updateSecCount 4
    "theorem" -> updateThmCount 1 ch
    "lemma" -> updateThmCount 2 ch
    "definition" -> updateThmCount 3 ch
    "example" -> updateThmCount 4 ch
    "exer" -> updateThmCount 5 ch
    "eq" -> updateThmCount 6 ch
    "code" -> updateThmCount 7 ch
    "figure" -> updateThmCount 8 ch
    "corollary" -> updateThmCount 9 ch
    "footnote" -> updateContCount 1 ch
    "attrs" -> updateContCount 2 ch
    "index" -> updateContCount 3 ch

  return (ids, root)

updateSecCountT :: Int -> State ([Int], [Int], [Int]) ([Int], [Int]) --
updateSecCountT hd = do
  count <- get
  let ([a, b, c, d], thmcnt, contcnt) = count
  let (counter, ids) = case hd of
                          1 -> ([a, 0, 0, 0], [a])
                          2 -> ([a, b+1, 0, 0], [a, b+1])
                          3 -> ([a, b, c+1, 0], [a, b, c+1])
                          4 -> ([a, b, c, d+1], [a, b, c, d+1])
  put (counter, thmcnt, contcnt)
  return (ids, counter)

updateThmCountT :: Int -> Int -> State ([Int], [Int], [Int]) ([Int], [Int])
updateThmCountT t ch = do
  count <- get
  let (seccnt, [th, lm, df, ep, ec, eq, cd, fg, co], contcnt) = count
  let (counter, ids) = case t of
                        1 -> ([th+1, lm, df, ep, ec, eq, cd, fg, co], [ch, th+1]) -- theorem
                        2 -> ([th, lm+1, df, ep, ec, eq, cd, fg, co], [ch, lm+1]) -- lemma
                        3 -> ([th, lm, df+1, ep, ec, eq, cd, fg, co], [ch, df+1]) -- definition
                        4 -> ([th, lm, df, ep+1, ec, eq, cd, fg, co], [ch, ep+1]) -- example
                        5 -> ([th, lm, df, ep, ec+1, eq, cd, fg, co], [ch, ec+1]) -- exercise
                        6 -> ([th, lm, df, ep, ec, eq+1, cd, fg, co], [ch, eq+1]) -- equation
                        7 -> ([th, lm, df, ep, ec, eq, cd+1, fg, co], [ch, cd+1]) -- code
                        8 -> ([th, lm, df, ep, ec, eq, cd, fg+1, co], [ch, fg+1]) -- figure
                        9 -> ([th, lm, df, ep, ec, eq, cd, fg, co+1], [ch, co+1]) -- corollary
  put (seccnt, counter, contcnt)
  return (ids, counter)

updateContCountT :: Int -> Int -> State ([Int], [Int], [Int]) ([Int], [Int])
updateContCountT t ch = do
  count <- get
  let (seccnt, thmcnt, [fn, attr, idx]) = count
  let (counter, ids) = case t of
                  1 -> ([fn+1, attr, idx], [ch, fn+1])
                  2 -> ([fn, attr+1, idx], [ch, attr+1])
                  3 -> ([fn, attr, idx+1], [ch, idx+1])
  put (seccnt, thmcnt, counter)
  return (ids, counter)

incrementT :: Text -> State ([Int], [Int], [Int]) ([Int], [Int])
incrementT t = do
  counter <- get
  let (seccnt, thmcnt, fncnt) = counter
  let ch = Prelude.head seccnt
  (ids, root) <- case t of
    "chapter" -> updateSecCountT 1
    "section" -> updateSecCountT 2
    "subsection" -> updateSecCountT 3
    "subsubsection" -> updateSecCountT 4
    "theorem" -> updateThmCountT 1 ch
    "lemma" -> updateThmCountT 2 ch
    "definition" -> updateThmCountT 3 ch
    "example" -> updateThmCountT 4 ch
    "exer" -> updateThmCountT 5 ch
    "eq" -> updateThmCountT 6 ch
    "code" -> updateThmCountT 7 ch
    "figure" -> updateThmCountT 8 ch
    "corollary" -> updateThmCountT 9 ch
    "footnote" -> updateContCountT 1 ch
    "attrs" -> updateContCountT 2 ch
    "index" -> updateContCountT 3 ch

  return (ids, root)


--([1,2,0,0], "header", "sec:sec2") : [([1,1], "TheEx", "theorem:th1"), ([1,1,0,0], "header", "sec:sec1"), ([1,0,0,0], "header" , "ch:ch1")]

htmlLabel :: Handle -> Doc -> StateT ([Int], [Int], [Int]) IO ()
htmlLabel h (Doc _ blocks) = labelBlocks h blocks

labelBlocks :: Handle -> Blocks -> StateT ([Int], [Int], [Int]) IO ()
--labelBlocks h Empty      = do return []
--labelBlocks h (b :<| bs) = do
  --dict <- labelBlock h b
  --dicts <- labelBlocks h bs
  --return $ dict ++ dicts
labelBlocks h = mapM_ (labelBlock h)

labelBlock :: Handle -> Block -> StateT ([Int], [Int], [Int]) IO ()
labelBlock h (Para (Attrs attrs :<| is)) = do
  labelHdParaHeader h attrs
  labelInlines h is

labelBlock h (Para is) = do
  labelInlines h is

labelBlock h (Header hd attrs is) = do
  labelHeader h hd attrs is

labelBlock h (DIV attrs bs)
     | [] <- cls     = do
        labelBlocks h bs
     | (c:cs) <- cls = do
        labelDIV h c cs ids avs bs
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

labelBlock h _ = do return ()


labelDIV :: Handle -> Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> StateT ([Int], [Int], [Int]) IO ()
labelDIV h c cs ids avs bs | c `elem` thmEnvs = do
  counter <- increment c
  let (val, count) = counter
  liftIO $ hPrint h (Prelude.head ids, val, c, count)
  labelBlocks h bs
  where thmEnvs = ["theorem", "lemma", "definition", "example", "exer"]

labelDIV h _ _ _ _ bs = do labelBlocks h bs

labelHeader :: Handle -> Int -> [Attr] -> Inlines -> StateT ([Int], [Int], [Int]) IO ()
labelHeader h hd attrs is = do
  let t = case hd of
          1 -> "chapter"
          2 -> "section"
          3 -> "subsection"
          4 -> "subsubsection"
  counter <- increment t
  let (ids, count) = counter
  if null (attrsId attrs)
    then liftIO $ hPrint h ("head", ids, "", count)
    else liftIO $ hPrint h (Prelude.head (attrsId attrs), ids, t, count)
  labelInlines h is


labelInlines :: Handle -> Inlines -> StateT ([Int], [Int], [Int]) IO ()
labelInlines h Empty      = do return ()
labelInlines h (i :<| is) = do
  labelInline h i
  labelInlines h is

labelInline :: Handle -> Inline -> StateT ([Int], [Int], [Int]) IO ()
labelInline h (Footnote is) = do
  counter <- increment "footnote"
  let (ids, count) = counter
  let dict = ("fn:fn", ids, "footnote", count)
  liftIO $ hPrint h dict
  labelInlines h is

labelInline h (Attrs attrs) = do
  counter <- increment "attrs"
  let (ids, count) = counter
  if null (attrsId attrs)
    then liftIO $ hPrint h ("InlineAttrs", ids, "", count)
    else liftIO $ hPrint h (Prelude.head (attrsId attrs), ids, "attrs", count)

labelInline h _ = return ()


labelHdParaHeader :: Handle -> [Attr] -> StateT ([Int], [Int], [Int]) IO ()
labelHdParaHeader h attrs = do
  counter <- increment "attrs"
  let (ids, count) = counter
  if null (attrsId attrs)
    then liftIO $ hPrint h [("hdPara", ids, "", count)]
    else liftIO $ hPrint h (Prelude.head (attrsId attrs), ids, "attrs", count)


htmlTest :: Doc -> State ([Int], [Int], [Int]) [DictState]
htmlTest (Doc _ blocks) = do
  testBlocks blocks

testBlocks :: Blocks -> State ([Int], [Int], [Int]) [DictState]
testBlocks Empty      = do return []
testBlocks (b :<| bs) = do
  dict <- testBlock b
  dicts <- testBlocks bs
  return $ dict ++ dicts


testBlock :: Block -> State ([Int], [Int], [Int]) [DictState]
testBlock (Para (Attrs attrs :<| is)) = do
  dict <- testHdParaHeader attrs
  dict' <- testInlines is
  return $ dict ++ dict'

testBlock (Para is) = do
  testInlines is

testBlock (Header hd attrs is) = do
  testHeader hd attrs is

testBlock (CodeBlock attrs txt) = testCode cls ids avs txt
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

testBlock (DIV attrs bs)
     | [] <- cls     = do
        testBlocks bs
     | (c:cs) <- cls = do
        testDIV c cs ids avs bs
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

testBlock _ = do return []


testDIV :: Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> State ([Int], [Int], [Int]) [DictState]
testDIV c cs ids avs bs | c `elem` thmEnvs = do
  counter <- incrementT c
  let (val, count) = counter
  dicts <- testBlocks bs
  if null ids
    then return dicts
    else return $ (Prelude.head ids, val, c, count) : dicts
  where thmEnvs = ["theorem", "lemma", "definition", "example", "exer", "figure", "corollary"]
{-
testDIV "exlist" _ _ _ bs = do testBlocks bs


testDIV "exer" _ ids _ bs = do
  counter <- increment "exercise"
  let (val, count) = counter
  dicts <- testBlocks bs
  if null ids
    then return dicts
    else return $ (Prelude.head ids, val, "exercise", count) : dicts

testDIV "exans" _ _ _ bs = do testBlocks bs

testDIV "answer" _ _ _ _ = do return []

 -- catch-all case.
 -- possible instances: example, answer.

testDIV c cs ids avs bs = do
  counter <- increment c
  let (val, count) = counter
  dicts <- testBlocks bs
  if null ids
    then return dicts
    else return $ (Prelude.head ids, val, c, count) : dicts

-}

testDIV _ _ _ _ bs = do testBlocks bs

testHeader :: Int -> [Attr] -> Inlines -> State ([Int], [Int], [Int]) [DictState]
testHeader hd attrs (i :<| is) = do
  let t = case hd of
          1 -> "chapter"
          2 -> "section"
          3 -> "subsection"
          4 -> "subsubsection"
  --mapM_ addDict h
  --dict <- mapM (addDict t) (attrsId attrs)
  counter <- incrementT t
  let (ids, count) = counter
  dicts <- testInlines is
  if null (attrsId attrs)
    then return dicts
    --then return $ ("head", ids, "", count) : dicts
    else return $ (Prelude.head (attrsId attrs), ids, t, count) : dicts

testInlines :: Inlines -> State ([Int], [Int], [Int]) [DictState]
testInlines Empty      = do return []
testInlines (i :<| is) = do
      dict <- testInline i
      dicts <- testInlines is
      return $ dict ++ dicts

testInline :: Inline -> State ([Int], [Int], [Int]) [DictState]
testInline (Footnote is) = do
  counter <- incrementT "footnote"
  let (ids, count) = counter
  let dict = ("fn:fn", ids, "footnote", count)
  dicts <- testInlines is
  return (dict:dicts)

testInline (Attrs attrs) = do
  counter <- incrementT "attrs"
  let (ids, count) = counter
  if null (attrsId attrs)
    then return []
    --then return [("InlineAttrs", ids, "", count)]
    else return [(Prelude.head (attrsId attrs), ids, "attrs", count)]

{-
testInline (Index idx) = do
  counter <- incrementT "index"
  let (ids, count) = counter
  let dict = (idx, ids, "index", count)
  return [dict]
-}

testInline (Index idx) = do return []

--testInline (CiteT ref Nothing) = latexCmd h "citet1" ref


testInline _ = return []


testHdParaHeader :: [Attr] -> State ([Int], [Int], [Int]) [DictState]
testHdParaHeader attrs = do
  counter <- incrementT "attrs"
  let (ids, count) = counter
  if null (attrsId attrs)
    then return []
    --then return [("hdPara", ids, "", count)]
    else return [(Prelude.head (attrsId attrs), ids, "attrs", count)]

testCode :: [Text] -> [Text] -> [(Text, Text)] -> Text -> State ([Int], [Int], [Int]) [DictState]
testCode cls is _ txt | "spec" `elem` cls = do
  counter <- incrementT "code"
  let (ids, count) = counter
  if null is then return []
      --then return [("hdPara", ids, "", count)]
    else return [(Prelude.head is, ids, "code", count)]

testCode ("texonly" : _) is _ txt = do
  counter <- incrementT "eq"
  let (ids, count) = counter
  if null is then return []
      --then return [("hdPara", ids, "", count)]
    else return [(Prelude.head is, ids, "eq", count)]

testCode cls is _ _ | "equation" `elem` cls = do
  counter <- incrementT "eq"
  let (ids, count) = counter
  if null is then return []
      --then return [("hdPara", ids, "", count)]
    else return [(Prelude.head is, ids, "eq", count)]

testCode _ _ _ _ = return []
