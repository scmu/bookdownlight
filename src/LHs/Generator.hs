module LHs.Generator where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import qualified System.IO as IO
import qualified Data.ByteString as BS (ByteString, readFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Cheapskate
import LHs.Render

import Development.Shake.FilePath

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

printLHs :: Handle -> Text -> IO ()
printLHs h = lhsRender h . markdown def

genLHs :: String -> String -> String -> IO ()
genLHs mdname lhsname tmpls = do
    hdl <- openFile lhsname WriteMode
    readFile lhsHeader >>= TIO.hPutStr hdl
    readFile mdname >>= printLHs hdl
    hClose hdl
  where lhsHeader = tmpls </> "lhsheader.lhs"
