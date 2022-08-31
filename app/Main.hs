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
import Book.TexRender
import Html.HtmlRender

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

        -- want [texBase </> "fpbook.pdf"]
        want [ htmlBase </> "Chapters/Introduction.html"]
        want [ htmlBase </> "Chapters/Basics.html"]
        want [ htmlBase </> "Chapters/Induction.html"]
        want [ htmlBase </> "Chapters/Semantics.html"]
        want [ htmlBase </> "Chapters/Derivation.html"]
        want [ htmlBase </> "Chapters/Folds.html"]

-- genHtmls
        forM_ chapters (\ch ->
          (htmlChs </> ch <.> "html") %> \htmlName -> do
            let mdName = contents </> ch <.> "md"
            need [mdName]
            putInfo ("# md->html (for " ++ htmlName ++ ")")
            liftIO (genHtmls mdName htmlName))

        -- forM_ chapters (\ch ->
        --   (lhsChs </> ch <.> "lhs") %> \lhsName -> do
        --     let mdName = contents </> ch <.> "md"
        --     need [mdName]
        --     putInfo ("# md->lhs (for " ++ lhsName ++ ")")
        --     liftIO (genLHs mdName lhsName))
        --
        -- forM_ chapters (\ch ->
        --   (texChs </> ch <.> "tex") %> \texName -> do
        --     let lhsName = lhsChs </> ch <.> "lhs"
        --     need [lhsName]
        --     command_ [Cwd texBase, FileStdout texName]
        --        "lhs2TeX" [".." </> lhsName])
        --
        -- (texBase </> "fpbook.pdf") %> \out -> do
        --    need (map (\ch -> texChs </> ch <.> "tex") chapters)
        --    need [texBase </> "fpbook.tex"]
        --    command_ [Cwd texBase]
        --        "xelatex" ["fpbook"]

genLHs :: String -> String -> IO ()
genLHs mdname lhsname = do
    hdl <- openFile lhsname WriteMode
    readFile lhsHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handle hdl
    hClose hdl
  where lhsHeader = tmpls </> "lhsheader.lhs"

--- Update
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

handle :: Handle -> Text -> IO ()
handle h = texRender h . markdown def

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

chapters :: [String]
chapters = [ "Introduction"
           , "Basics"
           , "Induction"
           , "Semantics"
           , "Derivation"
           , "Folds"
           , "SegProblems"
           ]
-- paths

contents = "contents"
texBase  = "tex"
lhsBase  = "lhs"
lhsChs   = lhsBase </> "Chapters"
texChs   = texBase </> "Chapters"
tmpls    = "templates"
htmlBase  =  "html"
htmlChs   =   htmlBase </> "Chapters"
