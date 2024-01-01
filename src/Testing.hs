module Testing where

import Prelude hiding (readFile)
import System.IO (stdout, IOMode(..), Handle)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Cheapskate
import Control.Monad.RWS
import Cheapskate.Parse
import Cheapskate.ParserCombinators
import Cheapskate.Util
import Cheapskate.Inlines
import Cheapskate.Types

import LHs.Render

handle :: Handle -> Text -> IO ()
handle h = lhsRender h . markdown def

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
