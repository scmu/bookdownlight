module Html.Generator where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO

import Control.Monad.State

import qualified Data.ByteString as BS (ByteString, readFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Cheapskate

import Html.Counter
import Html.Scanning

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
        in runState (scanDoc doc) (initChCounter i)


readHAux :: String -> IO (TOCIs, Dict)
readHAux hauxname = IO.readFile hauxname >>= (return . read)

genHtml :: String -> String -> String -> IO ()
genHtml mdname htmlname tmpls = do
    hdl <- openFile htmlname WriteMode
    readFile htmlHeader >>= TIO.hPutStr hdl
    readFile mdname >>= handleHtml hdl
    readFile htmlFooter >>= TIO.hPutStr hdl
    hClose hdl
  where htmlHeader = tmpls </> "htmlheader.html"
        htmlFooter = tmpls </> "htmlfooter.html"

handleHtml :: Handle -> Text -> IO ()
handleHtml h = undefined -- htmlRender h . markdown def
