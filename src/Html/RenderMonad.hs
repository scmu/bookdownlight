{-# LANGUAGE OverloadedStrings #-}
module Html.RenderMonad where

import System.IO (Handle)
import qualified System.IO as IO (hPutChar, hPutStr)

import Data.Text (Text)
import qualified Data.Text as Text (cons, append, pack)
import qualified Data.Text.IO as T (hPutStr)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import Cheapskate
import Syntax.Util

import Html.Types
import Html.Counter

runRMonad :: [Int] -> [String] -> Handle -> LblMap -> RMonad a -> IO a
runRMonad this allFileNames h lmap m =
  evalStateT (runReaderT m renv) (initChCounter (head this - 1))
 where renv = REnv this allFileNames h lmap

putStrR   xs = ReaderT (liftIO . flip IO.hPutStr xs . outHdlR)
putCharR  c  = ReaderT (liftIO . flip IO.hPutChar c . outHdlR)
putStrTR  xs = ReaderT (liftIO . flip T.hPutStr  xs . outHdlR)

currentCounters :: RMonad Counter
currentCounters = get

lookupLbl :: Text -> RMonad (Maybe RefNum)
lookupLbl lbl = reader (Map.lookup lbl . lMapR)

isThisFile :: Int -> RMonad Bool
isThisFile i = reader ((i==) . head . thisFileR)

chToFileName :: Int -> RMonad String
chToFileName i = reader (pick i . allFileNamesR)
   where pick i chs | i < length chs = chs !! i
                    | otherwise = ""


mkTag tag body = mkTagAttrsC tag ([], [], []) body

mkTagAttrs :: Text -> [Attr] -> RMonad () -> RMonad ()
mkTagAttrs tag attrs body =
  mkTagAttrsC tag (sortAttrs attrs) body

mkTagAttrsC :: Text -> AttrsC -> RMonad () -> RMonad ()
mkTagAttrsC tag attrs body = do
   putCharR '<' >> putStrTR tag >> renderAttrsC attrs >> putCharR '>'
   body
   putStrR "</" >> putStrTR tag >> putCharR '>'

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
