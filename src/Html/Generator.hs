{-# LANGUAGE OverloadedStrings #-}
module Html.Generator where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO

import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Reader

-- import Data.Binary (Binary(..), encodeFile, decodeFile)
import qualified Data.ByteString as BS (ByteString, readFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO

import Data.Map (Map)
import qualified Data.Map as Map

import Cheapskate

import Html.Types
import Html.Counter
import Html.Scanning
import Html.RenderMonad
import Html.Render
import Html.Pure

import Development.Shake.FilePath

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

--  SCM: used to generate binary encoded HAux files.
--       not used for now, since they turn out to be bigger than
--       files generated using simply show/read.
-- deriving instance Binary Attr
-- deriving instance Binary Inline

genHAux :: Int -> String -> String -> IO ()
genHAux i mdname hauxname = do
   --   -- SCM: binary encoded files turned out to be bigger!
   -- readFile mdname >>=
   --    (encodeFile hauxname . fst . mkHAux i)
   hdl <- openFile hauxname WriteMode
   readFile mdname >>= (IO.hPutStr hdl . show . fst . mkHAux i)
   hClose hdl

mkHAux :: Int -> Text -> (AuxInfo, Counter)
mkHAux i contents =
        let doc = markdown def $ contents
        in runState (scanDoc doc) (initChCounter (i-1))

readHAux :: String -> IO AuxInfo
readHAux hauxname = -- decodeFile -- SCM: binary encoded files turned out to be bigger!
  IO.readFile hauxname >>= (return . read)

genTOCLMap :: String -> IO (TOCIs, LblMap, IxMap)
genTOCLMap hauxname =
    mapTuple id Map.fromList ixMapFromList
      <$> readHAux hauxname

genTOCLMaps :: [String] -> IO (TOCIs, LblMap, IxMap)
genTOCLMaps hauxnames =
     ((mapTuple concat Map.unions unionIxMaps) . unzip3) <$>
      (mapM genTOCLMap hauxnames)

genHtml :: String -> String -> String
         -> (Int, [String], TOC, LblMap) -> IO ()
genHtml mdname htmlname tmpls (this, allFileNames, toc, lmap) = do
    hdl <- openFile htmlname WriteMode
    content <- readFile mdname
    runRMonad [this] allFileNames metaInfoFNames hdl lmap
       (do toc' <- renderTOCPartial toc
           mkPage tmpls toc' (htmlRender . markdown def $ content))
    hClose hdl

genTOC :: String -> TOC -> String -> ([String], LblMap) -> IO ()
genTOC tocFName toc tmpls (allFileNames, lmap) = do
  hdl <- openFile tocFName WriteMode
  runRMonad [-1] allFileNames metaInfoFNames hdl lmap
     (do toc' <- renderTOCPartial toc
         mkPage tmpls toc' (Just bookheader, renderTOCsList toc))
  hClose hdl
 where bookheader = do mkTag "h1" (putStrTR "函數程設與推論")
                       mkTag "h2" (putStrTR "Functional Program Construction and Reasoning")

genIx :: String -> IxMap -> String -> ([String], TOC, LblMap) -> IO ()
genIx ixFName ixMap tmpls (allFileNames, toc, lmap) = do
  hdl <- openFile ixFName WriteMode
  runRMonad [-1] allFileNames metaInfoFNames hdl lmap
     (do toc' <- renderTOCPartial toc
         mkPage tmpls toc' (Just (mkTag "h1" (putStrTR "索引")),
                            renderIx ixList))
  hClose hdl
 where ixList = Map.toAscList ixMap

tocFHtmlName :: String
tocFHtmlName = "TOC.html"

ixFHtmlName :: String
ixFHtmlName = "Ix.html"

type MetaInfoFNames = (String, String)
metaInfoFNames = (tocFHtmlName, ixFHtmlName)

mapTuple f g h (x, y, z) = (f x, g y, h z)
