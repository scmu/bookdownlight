module Html.Types where

import System.IO (Handle)
import Data.Text (Text)
import Data.Map (Map)

import Control.Monad.State
import Control.Monad.Reader

import Cheapskate

data Counter = Counter
      { chC     :: !Int
      , secC    :: !Int
      , subsecC :: !Int
      -- , subsubsecC :: !Int
      , thmC    :: !Int  -- shared by theorem, lemma, definition, example
      , figC    :: !Int
      , eqC     :: !Int
      , exerC   :: !Int
      , fnoteC  :: !Int
     }
  deriving Show

type RefNum = ([Int]    -- file identifier. non-empty
              ,[Int])   -- actual displayed number. non-empty

data Rose a = RNode a [Rose a]
     deriving Show

type TOCItem = ( RefNum     -- file id and section number
               , Inlines    -- title
               , Text       -- label
               )

type TOC = [Rose TOCItem]

-- partially rendered TOC

type PRTOC = [Rose ((Text, [Int]), RMonad (), Text)]

type LblMap = Map Text RefNum
---

data REnv = REnv { thisFileR     :: [Int]
                 , allFileNamesR :: [String]
                 , outHdlR       :: Handle
                 , lMapR         :: LblMap
                 }

type RMonad = ReaderT REnv (StateT Counter IO)
