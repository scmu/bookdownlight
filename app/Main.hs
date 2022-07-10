module Main where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import System.Directory
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text.IO as TIO

import Cheapskate
import Book.TexRender

main :: IO ()
main = -- generate lhs
    do projBase <- getCurrentDirectory
       putStr "base dir: "
       putStr projBase
       putStr "\n"
       mapM_ (genLHs (projBase ++ "/")) chapters

genLHs :: String -> String -> IO ()
genLHs base chname = do
    hdl <- openFile lhsname WriteMode
    readFile lhsHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handle hdl
    hClose hdl
  where mdname  = contents base ++ chname ++ ".md"
        lhsname = lhsChs base ++ chname ++ ".lhs"
        lhsHeader = tmpls base ++ "lhsheader.lhs"

handle :: Handle -> Text -> IO ()
handle h = texRender h . markdown def

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

chapters :: [String]
chapters = [ -- "Introduction"
           -- ,"Basics"
           -- ,"Induction"
           -- ,
           "Semantics"
           -- , "Derivation"
           -- , "Folds"
           ]
-- paths

-- projBase = "/Users/scm/Documents/Repositories/bookdownlight/"
contents projBase = projBase ++ "contents/"
texBase  projBase = projBase ++ "tex/"
lhsBase  projBase = projBase ++ "lhs/"
lhsChs   projBase = lhsBase projBase ++ "Chapters/"
texChs   projBase = texBase projBase ++ "Chapters/"
tmpls    projBase = projBase ++ "templates/"
