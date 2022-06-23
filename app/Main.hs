module Main where

import Prelude hiding (readFile)
import System.IO (openFile, hClose, stdout, IOMode(..), Handle)
import System.Directory
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Cheapskate

-- SCM: for testing
import Control.Monad.RWS
import Cheapskate.Parse
import Cheapskate.ParserCombinators
import Cheapskate.Util
import Cheapskate.Inlines
import Cheapskate.Types

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

chapters :: [String]
chapters = [ -- "Introduction"
           --,
             "Basics"
           -- , "Induction"
           -- , "Semantics"
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


-- tests

readFile :: String -> IO Text
readFile path = decodeUtf8 <$> BS.readFile path

textstF :: String -> IO ()
textstF file = readFile file >>= handle stdout
            -- T.getContents >>= handle
            -- T.getContents >>= (print . markdown def)

mdtstF :: String -> IO ()
mdtstF file =
  do contents <- readFile file
     print . markdown def $ contents

pltstF :: String -> IO ()
pltstF file =
  do contents <- readFile file
     print . processLines' $ contents

pltst :: String -> IO ()
pltst = print . processLines . T.pack

pldtstF :: String -> IO ()
pldtstF file =
  do contents <- readFile file
     print . processDocument . processLines $ contents

pldtst :: String -> IO ()
pldtst = print . processDocument . processLines . T.pack

-- processLines' :: Text -> (Container, ReferenceMap)
processLines' :: Text -> ContainerStack
processLines' t = stack
  where
  (_, stack, refmap) = runRWS (mapM_ processLine lns) () startState
  lns        = zip [1..] (map tabFilter $ T.lines t)
  startState = ContainerStack (Container Document mempty) []

{-
processLines :: Text -> (Container, ReferenceMap)
processLines t = (doc, refmap)
  where
  (doc, refmap) = evalRWS (mapM_ processLine lns >> closeStack) () startState
  lns        = zip [1..] (map tabFilter $ T.lines t)
  startState = ContainerStack (Container Document mempty) []
-}
