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
    need ( (htmlChs </> "TOC" <.> "html")
         : (htmlChs </> "Ix"  <.> "html")
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

 forM_ (zip [0..] chapters) (\(i,ch) ->
  (tmp </> "html" </> ch <.> "haux") %> \hauxName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->haux (for " ++ hauxName ++ ")")
   liftIO (genHAux i mdName hauxName))

 buildTOCLMap <- newCache $ \() -> do
   let hauxNames = [ tmp </> "html" </> ch <.> "haux" | ch <- chapters]
   need hauxNames
   putInfo ("# building TOC and label map from " ++ show hauxNames)
   mapTuple (buildRose 1) id id <$>
       liftIO (genTOCLMaps hauxNames)

 forM_ (zip [0..] chapters) (\(i,ch) ->
  (htmlChs </> ch <.> "html") %> \htmlName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# md->html (for " ++ htmlName ++ ")")
   liftIO (genHtml mdName htmlName tmpls
             (i, chapters, toc, lblMap)))

 htmlChs </> "TOC" <.> "html" %> \tocFName -> do
   (toc, lblMap, _) <- buildTOCLMap ()
   putInfo ("# generating TOC.html")
   liftIO (genTOC tocFName toc tmpls (chapters, lblMap))

 htmlChs </> "Ix" <.> "html" %> \ixFName -> do
   (toc, lblMap, ix) <- buildTOCLMap ()
   putInfo ("# generating Ix.html")
   liftIO (genIx ixFName ix tmpls (chapters, toc, lblMap))

-- configuration info.

chapters :: [String]
chapters = [ "Introduction"
           , "Basics"
           , "Induction"
           , "SearchTrees"
           , "Semantics"
           , "Derivation"
           , "Folds"
           , "SegProblems"
           , "Monads"
           ]
-- paths

root      = "fpcr"
contents  = root </> "contents"

texBase   = root </> "tex"
lhsBase   = root </> "lhs"
htmlBase  = root </> "html"
lhsChs    = lhsBase  </> "Chapters"
texChs    = texBase  </> "Chapters"
htmlChs   = htmlBase </> "Chapters"

tmpls     = root </> "templates"
tmp       = root </> "tmp"
