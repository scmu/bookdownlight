{-# LANGUAGE OverloadedStrings #-}

module Html.Scanning where

import Data.Sequence (Seq(..))
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Control.Arrow ((***), second)
import Cheapskate
import Syntax.Util

import Html.Counter

type LMonad a = State Counter a

type TOCIs = [(Int, ( [Int]   -- section number
                    , Inlines    -- title
                    , Text    -- label
                    ))]
type Dict  = [(Text, [Int])]

scanDoc :: Doc -> LMonad (TOCIs, Dict)
scanDoc (Doc _ bs) = scanBlocks bs

scanBlocks :: Blocks -> LMonad (TOCIs, Dict)
scanBlocks bs = ((concat *** concat) . unzip . toList) <$> mapM scanBlock bs

scanBlock :: Block -> LMonad (TOCIs, Dict)
scanBlock (Para is) = scanInlines is

scanBlock (Para (Attrs attrs :<| is)) =
  -- labelHdParaHeader h attrs
  scanInlines is

scanBlock (Header hd attrs is) =
  scanHeader hd attrs is

scanBlock (DIV attrs bs)
         | []    <- cls = scanBlocks bs
         | (c:_) <- cls = scanDIV c ids bs
      where ids = attrsId attrs
            cls = attrsClass attrs
            -- avs = attrsAVs attrs

scanBlock _ = return ([], [])

scanHeader :: Int -> [Attr] -> Inlines -> LMonad (TOCIs, Dict)
scanHeader hd attrs is = do
  nums <- state (newHeader hd)
  (ts, ds) <- scanInlines is
  return ((hd, (nums, is, idn)) : ts
         , map (\lbl -> (lbl, nums)) ids ++ ds)
 where ids = attrsId attrs
       idn = if null ids then "" else head ids

scanDIV :: Text -> [Text] -> Blocks -> LMonad (TOCIs, Dict)
scanDIV c ids bs | c `elem` thmEnvs = do
   nums <- state newThm
   second (map (\lbl -> (lbl, nums)) ids ++) <$> scanBlocks bs
  where thmEnvs = ["theorem", "lemma", "definition", "corollary", "example"]
scanDIV c ids bs | c == "exer" = do
   nums <- state newExer
   second (map (\lbl -> (lbl, nums)) ids ++) <$> scanBlocks bs
scanDIV _ _ bs = scanBlocks bs

scanInlines :: Inlines -> LMonad (TOCIs, Dict)
scanInlines is = return ([], [])
-- scanInlines is = concat <$> mapM scanInline is

scanInline :: Inline -> LMonad (TOCIs, Dict)
scanInline _ = return ([], [])  -- to be dealt with later.
