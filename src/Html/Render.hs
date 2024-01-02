module Html.Render where

import System.IO (hPutChar, hPutStr, Handle, hPrint)
import Data.Sequence (Seq(..))
import Data.Text (Text, head)
import qualified Data.Text.IO as T
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Trans.Reader

import Cheapskate

import Syntax.Util

import Html.Counter

type RMonad = ReaderT LblMap (StateT Counter IO)

htmlRender :: Handle -> Doc -> RMonad ()
htmlRender h (Doc _ blocks) = renderBlocks h blocks

renderBlocks :: Handle -> Blocks -> RMonad ()
renderBlocks h = mapM_ (renderBlock h)

renderBlock = undefined
