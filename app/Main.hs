{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Main where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO
import System.Directory
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text.IO as TIO
import Control.Arrow ((***))
import Control.Monad (forM_)

import Config

import Cheapskate

import LHs.Generator
import Html.Generator
import Html.Counter (buildRose)

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

{-

invoke by "stack ghci"

issue the command
  :main "goal"
in ghci, where "goal" can be "pdf", "html", or any filename.

-}

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  phonies
  lhsRules
  htmlRules

phonies :: Rules ()
phonies = do
  phony "lhs" $
    need ( (lhsChs </> fileName Preface <.> "lhs")
         : map (\ch -> lhsChs </> ch <.> "lhs") chapters)
  phony "tex" $
    need ( (texChs </> fileName Preface <.> "tex")
         : map (\ch -> texChs </> ch <.> "tex") chapters)
  phony "pdf" $
    need [texBase </> "fpcr.pdf"]
  phony "html" $
    need ( htmlNamePath Index
         : htmlNamePath ToC
         : htmlNamePath Preface
         : htmlNamePath Ix
         : htmlNamePath Biblio
         : map (\ch -> htmlChs </> ch <.> "html") chapters)

-- lhs, tex

lhsRules :: Rules ()
lhsRules = do

 forM_ (fileName Preface : chapters) (\ch ->
  (lhsChs </> ch <.> "lhs") %> \lhsName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->lhs (for " ++ lhsName ++ ")")
   liftIO (genLHs mdName lhsName (tmpls </> "lhs")))

 forM_ (fileName Preface : chapters) (\ch ->
  (texChs </> ch <.> "tex") %> \texName -> do
   let lhsName = lhsChs </> ch <.> "lhs"
   need [lhsName]
   command_ [Cwd texBase, FileStdout texName]
     "lhs2TeX" [".." </> ".." </> lhsName])

 (texBase </> "fpcr.pdf") %> \out -> do
  need (map (\ch -> texChs </> ch <.> "tex") (fileName Preface : chapters))
  need [texBase </> "fpcr.tex"]
  command_ [Cwd texBase] "xelatex" ["fpcr"]

 (texBase </> "fpcr.aux") %> \_ ->
  need [texBase </> "fpcr.pdf"]

-- html

htmlRules :: Rules ()
htmlRules = do

 forM_ [0..numOfChapters-1] (\i ->
   hauxNamePath i %> \hauxName -> do
   let mdName = mdNamePath (Chap [i])
   need [mdName]
   putInfo ("# md->haux (for " ++ hauxName ++ ")")
   liftIO (genHAux i mdName hauxName))

 buildTOCLMap <- newCache $ \() -> do
   let hauxNames = [ tmp </> "html" </> ch <.> "haux" | ch <- chapters]
   need hauxNames
   putInfo ("# building TOC and label map from " ++ show hauxNames)
   mapTuple (buildRose 1) id id <$>
       liftIO (genTOCLMaps hauxNames)

 buildBiblioMap <- newCache $ \() -> do
   let bibFileName = tmp </> "html" </> "reduced_sorted" <.> "bib"
   need [bibFileName]
   putInfo ("# parsing bibliography file")
   liftIO (genBiblioMap bibFileName)

 forM_ [0.. numOfChapters-1] (\i ->
  htmlNamePath (Chap [i]) %> \htmlName -> do
   let mdName = mdNamePath (Chap [i])
   need [mdName]
   (_, bibMap) <- buildBiblioMap ()
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# md->html (for " ++ htmlName ++ ")")
   liftIO (genChapterHtmls i toc lblMap bibMap))

 htmlNamePath Index %> \tocFName -> do
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# generating index.html")
   liftIO (genIndex toc lblMap)

 htmlNamePath Preface %> \tocFName -> do
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# generating Preface.html")
   liftIO (genPreface toc lblMap)

 htmlNamePath ToC %> \tocFName -> do
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# generating ToC.html")
   liftIO (genTOC toc lblMap)

 htmlNamePath Ix %> \ixFName -> do
   (toc, lblMap, ix) <- buildTOCLMap ()
   putInfo ("# generating Ix.html")
   liftIO (genIx ix toc lblMap)

 tmp </> "html" </> "reduced_sorted" <.> "bib" %> \_ -> do
   need [texBase </> "fpcr.aux"]
   putInfo ("# generating reduced and sorted bibliography file")
   command_ [Cwd texBase] "bibexport"
       [ "-n", "-ns"
       , "-o", (".." </> ".." </> tmp </> "html" </> "reduced_sorted_tmp" <.> "bib")
       , "fpcr.aux"]
   command_ [Cwd (tmp </> "html")] "bibtool"
       [ "reduced_sorted_tmp.bib"
       , "-s"
       , "-r", "../../templates/html_pure/fpcr-bibs.rsc"
       , "-o", "reduced_sorted.bib"]
  -- command_ [Cwd (tmp </> "html")] "rm" ["reduced_sorted_tmp.bib"]

 htmlNamePath Biblio %> \_ -> do
   (bib, _) <- buildBiblioMap ()
   (toc, _, _) <- buildTOCLMap ()
   putInfo ("# generating Biblio.html")
   liftIO (genBiblio bib toc)
