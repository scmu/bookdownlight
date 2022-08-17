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

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

        want [texBase </> "fpbook.pdf"]

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
           command_ [Cwd texBase]
               "xelatex" ["fpbook"]

genLHs :: String -> String -> IO ()
genLHs mdname lhsname = do
    hdl <- openFile lhsname WriteMode
    readFile lhsHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handle hdl
    hClose hdl
  where lhsHeader = tmpls </> "lhsheader.lhs"

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
