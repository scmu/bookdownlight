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
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO

import Data.Map (Map)
import qualified Data.Map as Map

import Text.BibTeX.Entry as BE

import Config
import Cheapskate

import Html.Types
import Html.Counter
import Html.Scanning
import Html.RenderMonad
import Html.Render
import Html.Pure
import Html.Bib

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

genHtml :: Int -> TOC -> LblMap -> BibMap -> IO ()
genHtml this toc lmap bmap = do
    hdl <- openFile (htmlNamePath (Chap [this])) WriteMode
    content <- readFile (mdNamePath this)
    runRMonad (Chap [this]) lmap bmap hdl
       (do toc' <- renderTOCPartial toc
           mkPage toc' (htmlRender . markdown def $ content))
    hClose hdl

genTOC :: TOC -> LblMap -> IO ()
genTOC toc lmap = do
  hdl <- openFile (htmlNamePath ToC) WriteMode
  runRMonad ToC lmap Map.empty hdl
     (do toc' <- renderTOCPartial toc
         mkPage toc' (Just bookheader, renderTOCsList toc))
  hClose hdl
 where bookheader = do mkTag "h1" (putStrTR "函數程設與推論")
                       mkTag "h2" (putStrTR "Functional Program Construction and Reasoning")

genIx :: IxMap -> TOC -> LblMap -> IO ()
genIx ixMap toc lmap = do
  hdl <- openFile (htmlNamePath Ix) WriteMode
  runRMonad Ix lmap Map.empty hdl
     (do toc' <- renderTOCPartial toc
         mkPage toc' (Just (mkTag "h1" (putStrTR "索引")),
                            renderIx ixList))
  hClose hdl
 where ixList = Map.toAscList ixMap

genBiblioMap :: FilePath -> IO ([BE.T], BibMap)
genBiblioMap fname = do
  bibs <- parseBib fname
  let bibMap = Map.fromList
                (map (\e -> (pack (BE.identifier e), e)) bibs)
  return (bibs, bibMap)

genBiblio :: [BE.T] -> TOC -> IO ()
genBiblio bib toc = do
 hdl <- openFile (htmlNamePath Biblio) WriteMode
 runRMonad Biblio Map.empty Map.empty hdl
    (do toc' <- renderTOCPartial toc
        mkPage toc' (Just (mkTag "h1" (putStrTR "參考書目")),
                           renderBib bib))
 hClose hdl

mapTuple f g h (x, y, z) = (f x, g y, h z)
