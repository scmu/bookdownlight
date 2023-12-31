module Main where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import System.Directory
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)

import Cheapskate
import LHs.LHsRender
import Html.HtmlRender
-- import Html.HtmlLabel

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
 -- want [texBase </> "fpbook.pdf"]
 -- want (map (\ch -> htmlChs </> ch <.> "html") chapters)

 phony "pdf" $ do
   need [texBase </> "fpbook.pdf"]

 phony "html" $ do
   need (map (\ch -> htmlChs </> ch <.> "html") chapters)

-- lhs, tex

 forM_ chapters (\ch ->
  (lhsChs </> ch <.> "lhs") %> \lhsName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->lhs (for " ++ lhsName ++ ")")
   liftIO (genLHs mdName lhsName))

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

 forM_ chapters (\ch ->
  (htmlChs </> ch <.> "html") %> \htmlName -> do
   let mdName = contents </> ch <.> "md"
   need [mdName]
   putInfo ("# md->html (for " ++ htmlName ++ ")")
   liftIO (genHtmls mdName htmlName))

genLHs :: String -> String -> IO ()
genLHs mdname lhsname = do
    hdl <- openFile lhsname WriteMode
    readFile lhsHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handleLHs hdl
    hClose hdl
  where lhsHeader = tmpls </> "lhsheader.lhs"

genHtmls :: String -> String -> IO ()
genHtmls mdname htmlname = do
    hdl <- openFile htmlname WriteMode
    readFile htmlHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handleHtml hdl
    readFile htmlFooter >>= TIO.hPutStr hdl
    hClose hdl
  where htmlHeader = tmpls </> "htmlheader.html"
        htmlFooter = tmpls </> "htmlfooter.html"

handleHtml :: Handle -> Text -> IO ()
handleHtml h = htmlRender h . markdown def

handleLHs :: Handle -> Text -> IO ()
handleLHs h = lhsRender h . markdown def

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

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

texBase  = "tex"
lhsBase  = "lhs"
htmlBase = "html"
lhsChs   = lhsBase  </> "Chapters"
texChs   = texBase  </> "Chapters"
htmlChs  = htmlBase </> "Chapters"

tmpls    = "templates"
