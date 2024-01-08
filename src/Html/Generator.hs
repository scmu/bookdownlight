module Html.Generator (
    module Html.Generator
  , LblMap
  ) where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.ByteString as BS (ByteString, readFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO

import Data.Map (Map)
import qualified Data.Map as Map

import Cheapskate

import Html.Counter
import Html.Scanning
import Html.Render

import Development.Shake.FilePath

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

initChCounter i = Counter i 0 0 0 0 0 0 0

genHAux :: Int -> String -> String -> IO ()
genHAux i mdname hauxname = do
   hdl <- openFile hauxname WriteMode
   readFile mdname >>= (IO.hPutStr hdl . show . fst . mkHAux i)
   hClose hdl

mkHAux :: Int -> Text -> ((TOCIs, Dict), Counter)
mkHAux i contents =
        let doc = markdown def $ contents
        in runState (scanDoc doc) (initChCounter (i-1))

readHAux :: String -> IO (TOCIs, Dict)
readHAux hauxname = IO.readFile hauxname >>= (return . read)

genLblMap :: String -> IO LblMap
genLblMap hauxname = do
    (_, dict) <- readHAux hauxname
    return (Map.fromList dict)

genLblMaps :: [String] -> IO LblMap
genLblMaps hauxnames = Map.unions <$> (mapM genLblMap hauxnames)

genHtml :: String -> String -> String
         -> (Int, [String], LblMap) -> IO ()
genHtml mdname htmlname tmpls (this, allFileNames, lmap) = do
    hdl <- openFile htmlname WriteMode
    readFile htmlHeader >>= TIO.hPutStr hdl
    readFile mdname >>= printHtml hdl (this, allFileNames, lmap)
    readFile htmlFooter >>= TIO.hPutStr hdl
    hClose hdl
  where htmlHeader = tmpls </> "htmlheader.html"
        htmlFooter = tmpls </> "htmlfooter.html"

printHtml :: Handle -> (Int, [String], LblMap)
          -> Text -> IO ()
printHtml h (this, allFileNames, lmap) content =
    evalStateT
      (runReaderT (htmlRender . markdown def $ content)
                  renv)
      (initChCounter (this-1))
  where renv = REnv [this] allFileNames h lmap
