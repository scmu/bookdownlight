{-# LANGUAGE OverloadedStrings #-}
module Html.Generator (
    module Html.Generator
  , LblMap
  ) where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO

import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Reader

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

genHAux :: Int -> String -> String -> IO ()
genHAux i mdname hauxname = do
   hdl <- openFile hauxname WriteMode
   readFile mdname >>= (IO.hPutStr hdl . show . fst . mkHAux i)
   hClose hdl

mkHAux :: Int -> Text -> ((TOCIs, Dict), Counter)
mkHAux i contents =
        let doc = markdown def $ contents
        in runState (scanDoc doc) (initChCounter (i-1))

readHAux :: String -> IO (TOCIs, Dict)
readHAux hauxname = IO.readFile hauxname >>= (return . read)

genTOCLMap :: String -> IO (TOCIs, LblMap)
genTOCLMap hauxname =
    (id *** Map.fromList) <$> readHAux hauxname

genTOCLMaps :: [String] -> IO (TOCIs, LblMap)
genTOCLMaps hauxnames =
     ((concat *** Map.unions) . unzip) <$>
      (mapM genTOCLMap hauxnames)

genHtml :: String -> String -> String
         -> (Int, [String], TOC, LblMap) -> IO ()
genHtml mdname htmlname tmpls (this, allFileNames, toc, lmap) = do
    hdl <- openFile htmlname WriteMode
    content <- readFile mdname
    runRMonad [this] allFileNames tocFHtmlName hdl lmap
       (do toc' <- renderTOCPartial toc
           mkPage tmpls toc' (htmlRender . markdown def $ content))
    hClose hdl

genTOC :: String -> TOC -> String -> ([String], LblMap) -> IO ()
genTOC tocFName toc tmpls (allFileNames, lmap) = do
  hdl <- openFile tocFName WriteMode
  runRMonad [-1] allFileNames tocFHtmlName hdl lmap
     (do toc' <- renderTOCPartial toc
         mkPage tmpls toc' (Just bookheader, renderTOCsList toc))
  hClose hdl
 where bookheader = do mkTag "h1" (putStrTR "函數程設與推論")
                       mkTag "h2" (putStrTR "Functional Program Construciton and Reasoning")

tocFHtmlName :: String
tocFHtmlName = "TOC.html"
