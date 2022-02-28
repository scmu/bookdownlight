module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
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
main = T.getContents >>= (print . markdown def)

handle :: Text -> IO ()
handle = texRender . markdown def

textst :: String -> IO ()
textst file = T.readFile file >>= handle

mdtst :: String -> IO ()
mdtst file =
  do contents <- T.readFile file
     print . markdown def $ contents

pltst :: String -> IO ()
pltst file =
  do contents <- T.readFile file
     print . processLines' $ contents

pldtst :: String -> IO ()
pldtst file =
  do contents <- T.readFile file
     print . processDocument . processLines $ contents

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
