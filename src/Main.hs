module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Cheapskate
import Book.TexRender

main :: IO ()
main = mdtst
     -- T.getContents >>= handle

handle :: Text -> IO ()
handle = texRender . markdown def

mdtst :: IO ()
mdtst = T.getContents >>= (print . markdown def)
