{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Main where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO
import System.Directory
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
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

 phony "pdf" $ do
   need [texBase </> "fpbook.pdf"]

 phony "html" $ do
   need (map (\ch -> htmlChs </> ch <.> "html") chapters)

 lhsRules

 htmlRules


-- lhs, tex

lhsRules :: Rules ()
lhsRules = do

 forM_ chapters (\ch ->
  (lhsChs </> ch <.> "lhs") %> \lhsName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->lhs (for " ++ lhsName ++ ")")
   liftIO (genLHs mdName lhsName tmpls))

 forM_ chapters (\ch ->
  (texChs </> ch <.> "tex") %> \texName -> do
   let lhsName = lhsChs </> ch <.> "lhs"
   need [lhsName]
   command_ [Cwd texBase, FileStdout texName]
     "lhs2TeX" [".." </> lhsName])

 (texBase </> "fpbook.pdf") %> \out -> do
  need (map (\ch -> texChs </> ch <.> "tex") chapters)
  need [texBase </> "fpbook.tex"]
  command_ [Cwd texBase] "xelatex" ["fpbook"]

-- html

htmlRules :: Rules ()
htmlRules = do

 forM_ (zip [-1..] chapters) (\(i,ch) ->
  (tmp </> ch <.> "haux") %> \hauxName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->haux (for " ++ hauxName ++ ")")
   liftIO (genHAux i mdName hauxName))

 buildLblMap <- newCache $ \() -> do
   let hauxNames = [ tmp </> ch <.> "haux" | ch <- chapters]
   need hauxNames
   liftIO (genLblMaps hauxNames)

 forM_ chapters (\ch ->
  (htmlChs </> ch <.> "html") %> \htmlName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->html (for " ++ htmlName ++ ")")
   liftIO (genHtml mdName htmlName tmpls))

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

contents = "contents"

texBase   = "tex"
lhsBase   = "lhs"
htmlBase  = "html"
lhsChs    = lhsBase  </> "Chapters"
texChs    = texBase  </> "Chapters"
htmlChs   = htmlBase </> "Chapters"

tmpls     = "templates"
tmp       = "tmp"
