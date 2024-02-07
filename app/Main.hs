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
    need (map (\ch -> lhsChs </> ch <.> "lhs") chapters)
  phony "tex" $
    need (map (\ch -> texChs </> ch <.> "tex") chapters)
  phony "pdf" $
    need [texBase </> "fpcr.pdf"]
  phony "html" $
    need ( htmlNamePath ToC
         : htmlNamePath Ix
         : map (\ch -> htmlChs </> ch <.> "html") chapters)

-- lhs, tex

lhsRules :: Rules ()
lhsRules = do

 forM_ chapters (\ch ->
  (lhsChs </> ch <.> "lhs") %> \lhsName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->lhs (for " ++ lhsName ++ ")")
   liftIO (genLHs mdName lhsName (tmpls </> "lhs")))

 forM_ chapters (\ch ->
  (texChs </> ch <.> "tex") %> \texName -> do
   let lhsName = lhsChs </> ch <.> "lhs"
   need [lhsName]
   command_ [Cwd texBase, FileStdout texName]
     "lhs2TeX" [".." </> ".." </> lhsName])

 (texBase </> "fpcr.pdf") %> \out -> do
  need (map (\ch -> texChs </> ch <.> "tex") chapters)
  need [texBase </> "fpcr.tex"]
  command_ [Cwd texBase] "xelatex" ["fpcr"]

-- html

htmlRules :: Rules ()
htmlRules = do

 forM_ [0..numOfChapters-1] (\i ->
   hauxNamePath i %> \hauxName -> do
   let mdName = mdNamePath i
   need [mdName]
   putInfo ("# md->haux (for " ++ hauxName ++ ")")
   liftIO (genHAux i mdName hauxName))

 buildTOCLMap <- newCache $ \() -> do
   let hauxNames = [ tmp </> "html" </> ch <.> "haux" | ch <- chapters]
   need hauxNames
   putInfo ("# building TOC and label map from " ++ show hauxNames)
   mapTuple (buildRose 1) id id <$>
       liftIO (genTOCLMaps hauxNames)

 forM_ [0.. numOfChapters-1] (\i ->
  htmlNamePath (Chap [i]) %> \htmlName -> do
   let mdName = mdNamePath i
   need [mdName]
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# md->html (for " ++ htmlName ++ ")")
   liftIO (genHtml i toc lblMap))

 htmlNamePath ToC %> \tocFName -> do
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# generating ToC.html")
   liftIO (genTOC toc lblMap)

 htmlNamePath Ix %> \ixFName -> do
   (toc, lblMap, ix) <- buildTOCLMap ()
   putInfo ("# generating Ix.html")
   liftIO (genIx ix toc lblMap)
