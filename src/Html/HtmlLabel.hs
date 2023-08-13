{-# LANGUAGE OverloadedStrings #-}
module Html.HtmlLabel where

import System.IO (hPutChar, hPutStr, Handle)
import Data.Sequence (Seq(..), index, drop)
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Cheapskate
import Syntax.Util
import Data.List (intersperse)


type SecCount = [Int]
type ThmCount = [Int]
type FNCount = [Int]

updateSecCount :: Int -> State ([Int], [Int], [Int]) ([Int], [Int]) --
updateSecCount hd = do
  count <- get
  let ([a, b, c, d], thmcnt, fncnt) = count
  let (counter, ids) = case hd of
                          1 -> ([a+1, 0, 0, 0], [a+1])
                          2 -> ([a, b+1, 0, 0], [a, b+1])
                          3 -> ([a, b, c+1, 0], [a, b, c+1])
                          4 -> ([a, b, c, d+1], [a, b, c, d+1])
  put (counter, thmcnt, fncnt)
  let root = case Prelude.head counter of
                1 -> "Introduction"
                2 -> "Basics"
                3 -> "Induction"
                4 -> "Semantics"
                5 -> "Derivation"
                6 -> "Folds"
                7 -> "SegProblems"
                8 -> "Monads"
  return (ids, counter)

{-
getChapter :: State SecCount Int
getChapter = do
  chapter <- gets Prelude.head
  return chapter
-}
--theorem", "lemma", "definition", "example

updateThmCount :: String -> Int -> State ([Int], [Int], [Int]) ([Int], [Int])
updateThmCount t ch = do
  count <- get
  let (seccnt, [chapter, th, lm, df, ep, ec], fncnt) = count
  let (counter, ids) =
                        if ch > chapter
                          then case t of
                            "theorem"    -> ([ch, 2, 1, 1, 1, 1], [ch, 1])
                            "lemma"      -> ([ch, 1, 2, 1, 1, 1], [ch, 1])
                            "definition" -> ([ch, 1, 1, 2, 1, 1], [ch, 1])
                            "example"    -> ([ch, 1, 1, 1, 2, 1], [ch, 1])
                            "exercise"   -> ([ch, 1, 1, 1, 1, 2], [ch, 1])
                          else case t of
                            "theorem"    -> ([ch, th+1, lm, df, ep, ec], [ch, th])
                            "lemma"      -> ([ch, th, lm+1, df, ep, ec], [ch, lm])
                            "definition" -> ([ch, th, lm, df+1, ep, ec], [ch, df])
                            "example"    -> ([ch, th, lm, df, ep+1, ec], [ch, ep])
                            "exercise"   -> ([ch, th, lm, df, ep, ec+1], [ch, ec])
  put (seccnt, counter, fncnt)
  let root = case ch of
                1 -> "Introduction"
                2 -> "Basics"
                3 -> "Induction"
                4 -> "Semantics"
                5 -> "Derivation"
                6 -> "Folds"
                7 -> "SegProblems"
                8 -> "Monads"
  return (ids, counter)


getTheorem :: State ThmCount Int
getTheorem = do
  th <- gets (\[_, th, _] -> th)
  return th

getExercise :: State ThmCount Int
getExercise = do
  ex <- gets (\[_, _, ex] -> ex)
  return ex

updateFNCount :: Int -> State ([Int], [Int], [Int]) ([Int], [Int])
updateFNCount ch = do
  counter <- get
  let (seccnt, thmcnt, [_, count]) = counter
  put (seccnt, thmcnt, [ch, count+1])
  let root = case ch of
                1 -> "Introduction"
                2 -> "Basics"
                3 -> "Induction"
                4 -> "Semantics"
                5 -> "Derivation"
                6 -> "Folds"
                7 -> "SegProblems"
                8 -> "Monads"
  return ([ch, count+1], [ch, count+1])

type DictState = (Text, [Int], Text, [Int]) -- (label, value, type, root) should be (key, value)
addDict :: Text -> Text -> State ([Int], [Int], [Int]) DictState
addDict t label = do
  counter <- get
  let (seccnt, thmcnt, fncnt) = counter
  let ch = Prelude.head seccnt
  (ids, root) <- case t of
                    "chapter"       -> updateSecCount 1
                    "section"       -> updateSecCount 2
                    "subsection"    -> updateSecCount 3
                    "subsubsection" -> updateSecCount 4
                    "theorem"       -> updateThmCount "theorem" ch
                    "lemma"         -> updateThmCount "lemma" ch
                    "definition"    -> updateThmCount "definition" ch
                    "example"       -> updateThmCount "example" ch
                    "exercise"      -> updateThmCount "exercise" ch
                    "footnote"      -> updateFNCount ch
                    "test"          -> updateFNCount ch
                    _               -> updateFNCount ch

  return (label, ids, t, root)
  --return (concat . intersperse "." . map show $ ids)


--([1,2,0,0], "header", "sec:sec2") : [([1,1], "TheEx", "theorem:th1"), ([1,1,0,0], "header", "sec:sec1"), ([1,0,0,0], "header" , "ch:ch1")]

--theorem, exercise, footnote
{-
getDict :: State DictState String
getDict = do
  (nums, _, _, _) <- gets Prelude.head
  return (concat . intersperse "." . map show $ nums)
-}
{-
htmlLabel :: Handle -> Doc -> State ([Int], [Int], [Int], DictState) DictState
htmlLabel h (Doc _ blocks) = do
  dict <- labelBlocks h blocks
  return dict

labelBlocks :: Handle -> Blocks -> State ([Int], [Int], [Int], DictState) DictState
labelBlocks h blocks = do
  dict <- mapM (labelBlock h) blocks
  return (concat dict)

labelBlock :: Handle -> Block -> State ([Int], [Int], [Int], DictState) DictState
labelBlock h (Header hd attrs is) = do
  dict <- labelHeader h hd attrs is
  return dict
labelBlock h (DIV attrs bs)
     | [] <- cls    = do
        dict <- labelBlocks h bs
        return dict
     | (c:cs) <- cls = do
        dict <- labelDIV h c cs ids avs bs
        return dict
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs
labelBlock h _ = do
  dicts <- get
  let (_, _, _, dict) = dicts
  return dict

labelDIV :: Handle -> Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> State ([Int], [Int], [Int], DictState) DictState
labelDIV h c cs ids avs bs | c `elem` thmEnvs = do
  dict <- forM ids $ \label ->
    addDict c label
--   dict <- labelBlocks h bs
  return (concat dict)
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example"]

labelDIV h "exer" _ ids _ bs = do
  dict <- forM ids $ \label ->
    addDict "exercise" label
  --labelBlocks h bs
  return (concat dict)

labelDIV h _ _ _ _ _ = do
  dicts <- get
  let (_, _, _, dict) = dicts
  return dict

labelHeader :: Handle -> Int -> [Attr] -> Inlines -> State ([Int], [Int], [Int], DictState) DictState
labelHeader h hd attrs is = do
  let t = case hd of
          1 -> "chapter"
          2 -> "section"
          3 -> "subsection"
          4 -> "subsubsection"
  --mapM_ addDict h
  dict <- forM (attrsId attrs) $ \label ->
    addDict t label
  return (concat dict)

labelInlines :: Handle -> Inlines -> State ([Int], [Int], [Int], DictState) DictState
labelInlines h inlines = do
  dict <- mapM (labelInline h) inlines
  return (concat dict)

labelInline :: Handle -> Inline -> State ([Int], [Int], [Int], DictState) DictState
labelInline h (Footnote is) = do
  dict <- addDict "footnote" "fn:fn"
     --labelInlines h is
  return dict
labelInline h _ = do
  dicts <- get
  let (_, _, _, dict) = dicts
  return dict
-}

htmlTest :: Doc -> State ([Int], [Int], [Int]) [DictState]
htmlTest (Doc _ blocks) = do
  dict <- testBlocks blocks
  return dict

testBlocks :: Blocks -> State ([Int], [Int], [Int]) [DictState]
testBlocks blocks = do
  if (null blocks)
    then return []
    else do
      dict <- testBlock (blocks `index` 0)
      dicts <- testBlocks (Data.Sequence.drop 1 blocks)
      return $ dict ++ dicts


testBlock :: Block -> State ([Int], [Int], [Int]) [DictState]
testBlock (Para (Attrs attrs :<| is)) = do
  dict <- hdParaHeader attrs
  dict' <- testInlines is
  return $ dict ++ dict'

testBlock (Para is) = do
  dict <- testInlines is
  return dict

testBlock (Header hd attrs is) = do
  dict <- testHeader hd attrs is
  return dict

testBlock (DIV attrs bs)
     | [] <- cls     = do
        dict <- testBlocks bs
        return dict
     | (c:cs) <- cls = do
        dict <- testDIV c cs ids avs bs
        return dict
  where ids = attrsId attrs
        cls = attrsClass attrs
        avs = attrsAVs attrs

testBlock _ = do return []


testDIV :: Text -> [Text] -> [Text] -> [(Text, Text)] -> Blocks -> State ([Int], [Int], [Int]) [DictState]
testlDIV c cs ids avs bs | c `elem` thmEnvs = do
  dict <- mapM (addDict c) ids
  dicts <- testBlocks bs
  return $ dict ++ dicts
 where thmEnvs :: [Text]
       thmEnvs = ["theorem", "lemma", "definition", "example"]

testDIV "exlist" _ _ _ bs = do
  dict <- testBlocks bs
  return dict

testDIV "exer" _ ids _ bs = do
  dict <- mapM (addDict "exercise") ids
  dicts <- testBlocks bs
  return $ dict ++ dicts

testDIV "exans" cs _ _ bs = do
  dict <- testBlocks bs
  return dict

 -- catch-all case.
 -- possible instances: example, answer.
testDIV c cs ids avs bs = do
  dict <- mapM (addDict c) ids
  dicts <- testBlocks bs
  return $ dict ++ dicts

--testDIV _ _ _ _ _ = do return []

testHeader :: Int -> [Attr] -> Inlines -> State ([Int], [Int], [Int]) [DictState]
testHeader hd attrs is = do
  let t = case hd of
          1 -> "chapter"
          2 -> "section"
          3 -> "subsection"
          4 -> "subsubsection"
  --mapM_ addDict h
  --dict <- mapM (addDict t) (attrsId attrs)
  dict <- mapM (addDict t) (attrsId attrs)
  dicts <- testInlines is
  return $ dict ++ dicts

testInlines :: Inlines -> State ([Int], [Int], [Int]) [DictState]
testInlines inlines = do
  if (null inlines)
    then return []
    else do
      dict <- testInline (inlines `index` 0)
      dicts <- testInlines (Data.Sequence.drop 1 inlines)
      return $ dict ++ dicts

testInline :: Inline -> State ([Int], [Int], [Int]) [DictState]
testInline (Footnote is) = do
  dict <- addDict "footnote" "fn:fn"
  dicts <- testInlines is
  return (dict:dicts)


testInline (Attrs attrs) = do
  dict <- mapM (addDict "test") (attrsId attrs)
  return dict

testInline _ = return []


hdParaHeader :: [Attr] -> State ([Int], [Int], [Int]) [DictState]
hdParaHeader attrs = do
  dict <- mapM (addDict "test") (attrsId attrs)
  return dict


