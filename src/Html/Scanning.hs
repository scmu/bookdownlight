{-# LANGUAGE OverloadedStrings #-}

module Html.Scanning where

import Data.Sequence (Seq(..))
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Control.Arrow ((***), second)
import Control.Applicative ((<|>))
import qualified Data.Map as M (Map(..), empty, alter, unionsWith)

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Cheapskate
import Syntax.Util

import Html.Types
import Html.Counter

type LMonad a = State Counter a

type TOCIs = [(Int, TOCItem)]
type Dict  = [(Text, RefNum)]
type IDict = [(Text,                -- name for looking up
                     ( Maybe Text   -- printed name
                     , Maybe Text   -- sub-category
                     , RefNum ))]

type AuxInfo = (TOCIs, Dict, IDict)

mapAxDict f (ts, ds, ixs) = (ts, f ds, ixs)

---

scanDoc :: Doc -> LMonad AuxInfo
scanDoc (Doc _ bs) = scanBlocks bs

scanBlocks :: Blocks -> LMonad AuxInfo
scanBlocks bs = (concat3 . unzip3 . toList) <$> mapM scanBlock bs
   where concat3 (xss, yss, zss) = (concat xss, concat yss, concat zss)

scanBlock :: Block -> LMonad AuxInfo
scanBlock (Para is) = scanInlines is

--scanBlock (Para (Attrs attrs :<| is)) =
  -- labelHdParaHeader h attrs
--  scanInlines is

scanBlock (Header hd attrs is) =
  scanHeader hd attrs is

scanBlock (DIV attrs bs)
         | []    <- cls = scanBlocks bs
         | (c:_) <- cls = scanDIV c ids bs
      where ids = attrsId attrs
            cls = attrsClass attrs
            -- avs = attrsAVs attrs

scanBlock _ = return ([], [], [])

scanHeader :: Int -> [Attr] -> Inlines -> LMonad AuxInfo
scanHeader hd attrs is = do
  nums <- state (newHeader hd)
  (ts, ds, ixs) <- scanInlines is
  return ((hd, (nums, is, idn)) : ts
         , map (\lbl -> (lbl, nums)) ids ++ ds
         , ixs)
 where ids = attrsId attrs
       idn = if null ids then "" else head ids

scanDIV :: Text -> [Text] -> Blocks -> LMonad AuxInfo
scanDIV c ids bs | c `elem` thmEnvs = do
   nums <- state newThm
   mapAxDict (map (\lbl -> (lbl, nums)) ids ++) <$> scanBlocks bs
  where thmEnvs = ["theorem", "lemma", "definition", "corollary", "example"]
scanDIV c ids bs | c == "exer" = do
   nums <- state newExer
   mapAxDict (map (\lbl -> (lbl, nums)) ids ++) <$> scanBlocks bs
scanDIV _ _ bs = scanBlocks bs

scanInlines :: Inlines -> LMonad AuxInfo
scanInlines is = do
  ixs <- concat <$> mapM scanInline is
  return ([], [], ixs)

  -- for now, inlines contain indices only

scanInline :: Inline -> LMonad IDict
scanInline (Index idx) = do
  ix <- state newIdx
  return [(term, (disp, sub, ix))]
 where (pre, bang, post) = (idx =~ ("[^\\\\](\\\\\\\\)*!" :: Text)) ::
                              (Text, Text, Text)
       (main, sub) | Text.null bang = (pre, Nothing)
                   | otherwise = (pre <> Text.init bang, Just post)
       (pre2, atat, post2) = main =~ ("@@{" :: Text) :: (Text, Text, Text)
       (term, disp) | Text.null atat = (main, Nothing)
                    | not (Text.null post2) =
                      (pre2, Just (Text.init post2))
                    | otherwise = (pre2, Just post2)

scanInline _ = return []

ixMapFromList :: IDict -> IxMap
ixMapFromList = foldl addIx M.empty
  where addIx m (term, (pname, subcat, rnum)) =
          M.alter (addIx' (pname, subcat, rnum)) term m
        addIx' (pname, Nothing, rnum)   Nothing = Just (pname, [rnum], [])
        addIx' (pname, Just subc, rnum) Nothing = Just (pname, [], [(subc, [rnum])])
        addIx' (pname, Nothing, rnum) (Just (pname', rs, subs)) =
            Just (pname <|> pname', rnum:rs, subs)
        addIx' (pname, Just subc, rnum) (Just (pname', rs, subs)) =
          Just (pname <|> pname', rs, insert (subc, rnum) subs)
        insert (st, r) [] = [(st, [r])]
        insert (st, r) ((s1,rs):rest) = case compare st s1 of
            LT -> (st, [r]) : (s1,rs) : rest
            EQ -> (st, r:rs) : rest
            GT -> (s1,rs) : insert (st,r) rest

unionIxMaps :: [IxMap] -> IxMap
unionIxMaps = M.unionsWith unionIx
  where unionIx (p1, rs1, ss1) (p2, rs2, ss2) =
           (p1 <|> p2, rs2 ++ rs1, merge ss1 ss2)  -- reverse order
        merge [] ss2 = ss2
        merge ss1 [] = ss1
        merge ((s1,rs1):ss1) ((s2,rs2):ss2) = case compare s1 s2 of
          LT -> (s1,rs1) : merge ss1 ((s2,rs2):ss2)
          EQ -> (s1,rs2++rs1) : merge ss1 ss2  -- reverse order
          GT -> (s2,rs2) : merge ((s1,rs1):ss1) ss2
