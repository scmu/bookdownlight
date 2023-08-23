module TestingHtml where

import Prelude hiding (readFile)
import System.IO (stdout, IOMode(..), Handle, openFile, writeFile, hPrint, hClose, hPutStr, hPutStrLn)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Trans

import Cheapskate
import Control.Monad.RWS
import Cheapskate.Parse
import Cheapskate.ParserCombinators
import Cheapskate.Util
import Cheapskate.Inlines
import Cheapskate.Types

import LHs.LHsRender
import Html.HtmlLabel

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
     let doc = markdown def $ contents
     let initState = ([1,0,0,0], [0,0,0,0,0], [0,0])
     print (runState (htmlTest doc) initState)

mdtStr :: String -> StateT ([Int], [Int], [Int]) IO [DictState]
mdtStr file = do
  currState <- get
  contents <- liftIO $ readFile file
  let doc = markdown def $ contents
  let (v, ([c, _, _, _], _, cont)) = runState (htmlTest doc) currState
  put ([c+1,0,0,0], [0,0,0,0,0], cont)
  return v


chapters :: [String]
chapters = [ "Introduction"
           , "Basics"
           , "Induction"
           , "Semantics"
           , "Derivation"
           , "Folds"
           , "SegProblems"
           ]

makeDict :: IO [DictState]
makeDict = do
  let initState = ([1,0,0,0], [0,0,0,0,0], [0,0])
  (dicts, _) <- foldM processChapter ([], initState) chapters
  hdl <- openFile "./src/Html/dict.hs" WriteMode
  hPutStrLn hdl "dict :: [DictState]"
  hPutStr hdl "dict = "
  hPrint hdl dicts
  hClose hdl
  return dicts

processChapter :: ([DictState], ([Int], [Int], [Int])) -> String -> IO ([DictState], ([Int], [Int], [Int]))
processChapter (dicts, s) chapter = do
  (chapterDict, newState) <- runStateT (mdtStr $ "./contents/" ++ chapter ++ ".md") s
  return (dicts ++ chapterDict, newState)



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
