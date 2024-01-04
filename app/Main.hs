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
import Control.Monad (forM_)

-- import Data.Binary        -- To use addOracle
-- import Data.Typeable
-- import Data.Hashable
-- import Control.DeepSeq

import Cheapskate

import LHs.Generator
import Html.Generator

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
    need (map (\ch -> htmlChs </> ch <.> "html") chapters)

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

 forM_ (zip [-1..] chapters) (\(i,ch) ->
  (tmp </> "html" </> ch <.> "haux") %> \hauxName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->haux (for " ++ hauxName ++ ")")
   liftIO (genHAux i mdName hauxName))

 buildLblMap <- newCache $ \() -> do
   let hauxNames = [ tmp </> "html" </> ch <.> "haux" | ch <- chapters]
   need hauxNames
   putInfo ("# building label map from " ++ show hauxNames)
   liftIO (genLblMaps hauxNames)

 forM_ chapters (\ch ->
  (htmlChs </> ch <.> "html") %> \htmlName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   lblMap <- buildLblMap ()
   putInfo ("# size of lmap: " ++ show (Map.size lblMap))
   putInfo ("# md->html (for " ++ htmlName ++ ")")
   liftIO (genHtml mdName htmlName (tmpls </> "html") lblMap))

-- newtype BuildLblMap = BuildLblMap ()
--   deriving (Show, Eq, Binary, NFData, Typeable, Hashable)
-- type instance RuleResult BuildLblMap = LblMap

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
