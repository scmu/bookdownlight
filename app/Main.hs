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

        want ["tex/fpbook.tex"]

        forM_ chapters (\ch ->
          ("lhs/Chapters/" </> ch <.> "lhs") %> \lhsName -> do
            let mdName = "contents" </> ch <.> "md"
            need [mdName]
            liftIO (genLHs mdName lhsName))

        forM_ chapters (\ch ->
          ("tex/Chapters/" </> ch <.> "tex") %> \texName -> do
            let lhsName = "lhs/Chapters/" </> ch <.> "lhs"
            need [lhsName]
            command_ [Cwd "tex", FileStdout texName]
               "lhs2TeX" [".." </> lhsName])

        "tex/fpbook.tex" %> \out -> do
           need (map (\ch -> "tex/Chapters/" </> ch <.> "tex") chapters)
           command_ [Cwd "tex"]
               "xelatex" ["fpbook"]

genLHs :: String -> String -> IO ()
genLHs mdname lhsname = do
    hdl <- openFile lhsname WriteMode
    readFile lhsHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handle hdl
    hClose hdl
  where lhsHeader = "templates/lhsheader.lhs"

handle :: Handle -> Text -> IO ()
handle h = texRender h . markdown def

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

chapters :: [String]
chapters = [ "Introduction"
           , "Basics"
           , "Induction"
           , "Semantics"
           -- , "Derivation"
           -- , "Folds"
           ]
-- paths

contents projBase = projBase ++ "contents/"
texBase  projBase = projBase ++ "tex/"
lhsBase  projBase = projBase ++ "lhs/"
lhsChs   projBase = lhsBase projBase ++ "Chapters/"
texChs   projBase = texBase projBase ++ "Chapters/"
tmpls    projBase = projBase ++ "templates/"
