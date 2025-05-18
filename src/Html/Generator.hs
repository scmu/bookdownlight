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

import Data.Sequence (Seq(..), breakl)

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
import Syntax.Util (attrsId)

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

{-
genHtml :: Int -> TOC -> LblMap -> BibMap -> IO ()
genHtml this toc lmap bmap = do
    hdl <- openFile (htmlNamePath (Chap [this])) WriteMode
    content <- readFile (mdNamePath (Chap [this]))
    runRMonad (Chap [this]) lmap bmap hdl
       (do toc' <- renderTOCPartial toc
           mkPage toc' (htmlRender . markdown def $ content))
    hClose hdl
-}

genChapterHtmls :: Int -> TOC -> [(Maybe TOCItem, Maybe TOCItem)]
                -> LblMap -> BibMap -> IO ()
genChapterHtmls this toc (cadj : sadj) lmap bmap = do
    content <- readFile (mdNamePath (Chap [this]))
    let (Doc _ blocks) = markdown def content
    let (chTitle, (chPremble, sections)) =
           (fmap (\ (Header _ attrs is) -> renderHeader 1 attrs is) ***
            splitSections) . sepChHeader $ blocks
    genChapter (chTitle, chPremble) cadj
    _ <- genSections chTitle sections (initChCounter this) sadj
    return ()
 where
  genChapter (chTitle, chPremble) adj = do
    hdl <- openFile (htmlNamePath (Chap [this])) WriteMode
    let adjM = (fmap renderTOCItemHRef *** fmap renderTOCItemHRef) adj
    runRMonad (Chap [this]) lmap bmap hdl
       (do toc' <- renderTOCPartial toc
           _ <- state incChap
           mkPage toc' (chTitle, renderBlocks chPremble) adjM)
    hClose hdl
  genSections chTitle [] cnt _ = return cnt
  genSections chTitle ((attrs, secTitle, secBody):secs) cnt (adj:sadj) = do
    let (ch, sec) = (chC cnt, secC cnt)
    hdl <- openFile (htmlNamePath (Chap [ch, sec+1])) WriteMode
    let adjM = (fmap renderTOCItemHRef *** fmap renderTOCItemHRef) adj
    (_,cnt') <- runRMonadWCounter (Chap [ch, sec+1]) lmap bmap hdl cnt
       (do toc' <- renderTOCPartial toc
           mkPage toc' (chTitle,
              renderBlocks (Header 2 attrs secTitle :<| secBody)) adjM)
    hClose hdl
    genSections chTitle secs cnt' sadj

genIndex :: TOC -> LblMap -> IO ()
genIndex toc lmap = do
  hdl <- openFile (htmlNamePath Index) WriteMode
  runRMonad Index lmap Map.empty hdl
     (do toc' <- renderTOCPartial toc
         mkPage toc' (Just bookheader, renderTOCsList toc) (Nothing, Nothing))
  hClose hdl
 where bookheader = do mkTag "h1" (putStrTR "函數程設與推論")
                       mkTag "h2" (putStrTR "Functional Program Construction and Reasoning")
genPreface :: TOC -> LblMap -> IO ()
genPreface toc lmap = do
  hdl <- openFile (htmlNamePath Preface) WriteMode
  content <- readFile (mdNamePath Preface)
  runRMonad Preface lmap Map.empty hdl
     (do toc' <- renderTOCPartial toc
         mkPage toc' (htmlRender . markdown def $ content) (Nothing, Nothing))
  hClose hdl
 where bookheader = do mkTag "h1" (putStrTR "函數程設與推論")
                       mkTag "h2" (putStrTR "Functional Program Construction and Reasoning")

genTOC :: TOC -> LblMap -> IO ()
genTOC toc lmap = do
  hdl <- openFile (htmlNamePath ToC) WriteMode
  runRMonad ToC lmap Map.empty hdl
     (do toc' <- renderTOCPartial toc
         mkPage toc' (Just bookheader, renderTOCsList toc) (Nothing, Nothing))
  hClose hdl
 where bookheader = do mkTag "h1" (putStrTR "函數程設與推論")
                       mkTag "h2" (putStrTR "Functional Program Construction and Reasoning")

genIx :: IxMap -> TOC -> LblMap -> IO ()
genIx ixMap toc lmap = do
  hdl <- openFile (htmlNamePath Ix) WriteMode
  runRMonad Ix lmap Map.empty hdl
     (do toc' <- renderTOCPartial toc
         mkPage toc' (Just (mkTag "h1" (putStrTR "索引")),
                            renderIx ixList) (Nothing, Nothing))
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
                           renderBib bib) (Nothing, Nothing))
 hClose hdl

mapTuple f g h (x, y, z) = (f x, g y, h z)

---
{-
sepChHeader :: Blocks -> (Maybe Block, Blocks)
sepChHeader Empty = (Nothing, Empty)
sepChHeader (Header 1 attrs is :<| blocks) =
  (Just (Header 1 attrs is), blocks)
sepChHeader (Header hd attrs is :<| blocks) =
  (Nothing, Header hd attrs is :<| blocks)
sepChHeader (bl :<| blocks) =
  (id *** (bl :<|)) $ sepChHeader blocks
-}
splitSections :: Blocks -> (Blocks, [([Attr], Inlines -- header info
                                     , Blocks)])      -- section body
splitSections Empty = (Empty, [])
splitSections blocks =
  let (premble, rest) = breakl isSectHeader blocks
  in (premble, splitSections' rest)

  -- inv: blocks either empty or starts with Header 2.
splitSections' :: Blocks -> [([Attr], Inlines, Blocks)]
splitSections' Empty = []
splitSections' (Header _ attrs title :<| rest) =
  let (sec, rest') = breakl isSectHeader rest
  in ((attrs, title, sec) : splitSections' rest')

isSectHeader (Header 2 _ _) = True
isSectHeader _              = False
