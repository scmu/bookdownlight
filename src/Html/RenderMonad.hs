{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Html.RenderMonad where

import System.IO (Handle)
import qualified System.IO as IO (hPutChar, hPutStr)

import Data.Text (Text)
import qualified Data.Text as Text (cons, append, pack)
import qualified Data.Text.IO as T (hPutStr)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Config
import Cheapskate
import qualified Text.BibTeX.Entry as BE
import Syntax.Util

import Html.Types
import Html.Counter

runRMonad :: FileRole -> LblMap -> BibMap -> Handle -> RMonad a -> IO a
runRMonad this lmap bmap h m =
  evalStateT (runReaderT m renv) (initChCounter chcounter)
 where renv = REnv this lmap bmap h
       chcounter = case this of
                     Chap (i:_)   -> i - 1
                     _            -> -1

runRMonadWCounter :: FileRole -> LblMap -> BibMap -> Handle
                  -> Counter -> RMonad a -> IO (a, Counter)
runRMonadWCounter this lmap bmap h cnt m =
  runStateT (runReaderT m renv) cnt
 where renv = REnv this lmap bmap h

putStrR   xs = ReaderT (liftIO . flip IO.hPutStr xs . outHdlR)
putCharR  c  = ReaderT (liftIO . flip IO.hPutChar c . outHdlR)
putStrTR  xs = ReaderT (liftIO . flip T.hPutStr  xs . outHdlR)

currentCounters :: RMonad Counter
currentCounters = get

lookupLbl :: Text -> RMonad (Maybe RefNum)
lookupLbl lbl = reader (Map.lookup lbl . lMapR)

lookupBib :: Text -> RMonad (Maybe (BE.T))
lookupBib idn = reader (Map.lookup idn . bMapR)

isThisFile :: FileRole -> RMonad Bool
isThisFile fr = reader ((fr ==) . thisFileR)

-- chToFileName :: [Int] -> RMonad String
-- chToFileName ch = reader (($ Chap ch) . fileNamesR)

mkTag :: Text -> RMonad a -> RMonad a
mkTag tag body = mkTagAttrsC tag ([], [], []) body

mkTagAttrs :: Text -> [Attr] -> RMonad a -> RMonad a
mkTagAttrs tag attrs body =
  mkTagAttrsC tag (sortAttrs attrs) body

mkTagAttrsC :: Text -> AttrsC -> RMonad a -> RMonad a
mkTagAttrsC tag attrs body = do
   putCharR '<' >> putStrTR tag >> renderAttrsC attrs >> putCharR '>'
   res <- body
   putStrR "</" >> putStrTR tag >> putCharR '>'
   return res

 -- self-closing tags

mkSCTag :: Text -> RMonad ()
mkSCTag tag = putCharR '<' >> putStrTR tag >> putStrTR "/>"

mkSCTagAttrsC :: Text -> AttrsC -> RMonad ()
mkSCTagAttrsC tag attrs =
  putCharR '<' >> putStrTR tag >> renderAttrsC attrs >> putStrTR "/>"

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
  where renderAttr (a, v)
         | v == ""   = do putCharR ' ' >> putStrTR a
         | otherwise = do putCharR ' ' >> putStrTR a >> putStrTR "=\""
                          putStrTR v >> putCharR '"'

--

instance Semigroup a => Semigroup (RMonad a) where
  m1 <> m2 = liftM2 (<>) m1 m2

instance Monoid a => Monoid (RMonad a) where
  mempty = return mempty
