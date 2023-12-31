{-# LANGUAGE OverloadedStrings #-}

module Html.CollectLabels where

import Data.Sequence (Seq(..), index, drop)
import Data.Text (Text, head, unpack)
import qualified Data.Text.IO as T
import Control.Monad (when)
import Control.Monad.State
import Cheapskate
import Syntax.Util

import Html.Counter

type LMonad a = State Counter a

type Dict = [(Text, [Int])]

labelDoc :: Doc -> LMonad ([TOC], Dict)
labelDoc (Doc _ bs) = labelBlocks bs

labelBlocks :: Blocks -> LMonad Dict
labelBlocks bs = concat <$> mapM labelBlock bs

labelBlock :: Block -> LMonad Dict
labelBlock (Para is) = labelInlines is

labelBlock (Para (Attrs attrs :<| is)) =
  -- labelHdParaHeader h attrs
  labelInlines is

labelBlock (Header hd attrs is) =
  labelHeader hd attrs is

labelBlock (DIV attrs bs)
         | []    <- cls = labelBlocks bs
         | (c:_) <- cls = labelDIV c ids bs
      where ids = attrsId attrs
            cls = attrsClass attrs
            -- avs = attrsAVs attrs

labelBlock _ = return []

labelHeader :: Int -> [Attr] -> Inlines -> LMonad Dict
labelHeader hd attrs is = do
  nums <- state (newHeader hd)
  (map (\lbl -> (lbl, nums)) ids ++) <$> labelInlines is
 where ids = attrsId attrs

labelDIV :: Text -> [Text] -> Blocks -> LMonad Dict
labelDIV c ids bs | c `elem` thmEnvs = do
   nums <- state newThm
   (map (\lbl -> (lbl, nums)) ids ++) <$> labelBlocks bs
  where thmEnvs = ["theorem", "lemma", "definition", "corollary", "example"]
labelDIV c ids bs | c == "exer" = do
   nums <- state newExer
   (map (\lbl -> (lbl, nums)) ids ++) <$> labelBlocks bs
labelDIV _ _ bs = labelBlocks bs

labelInlines :: Inlines -> LMonad Dict
labelInlines is = return []
-- labelInlines is = concat <$> mapM labelInline is

labelInline :: Inline -> LMonad Dict
labelInline _ = return []  -- to be dealt with later.
