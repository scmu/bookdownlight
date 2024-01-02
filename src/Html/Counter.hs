module Html.Counter where
import Data.Text (Text)
import Data.Map (Map)

data Counter = Counter
      { chC :: !Int
      , secC :: !Int
      , subsecC :: !Int
      -- , subsubsecC :: !Int
      , thmC  :: !Int  -- shared by theorem, lemma, definition, example
      , figC  :: !Int
      , eqC   :: !Int
      , exerC :: !Int
      , footnoteC :: !Int
     }
  deriving Show

initCounter :: Int -> Counter
initCounter i = Counter i 0 0 0 0 0 0 0

newChap :: Counter -> ([Int], Counter)
newChap cnt = ([chC cnt + 1],
               Counter (chC cnt + 1) 0 0 0 0 0 0 0)

newSec :: Counter -> ([Int], Counter)
newSec cnt = ([chC cnt', secC cnt'], cnt')
  where cnt' = cnt { secC = secC cnt + 1
                   , subsecC = 0}

newSubSec :: Counter -> ([Int], Counter)
newSubSec cnt = ([chC cnt', secC cnt', subsecC cnt'], cnt')
  where cnt' = cnt { subsecC = subsecC cnt + 1 }

newHeader :: Int -> Counter -> ([Int], Counter)
newHeader 1 = newChap
newHeader 2 = newSec
newHeader 3 = newSubSec

newThm :: Counter -> ([Int], Counter)
newThm cnt = ([chC cnt', thmC cnt'] , cnt')
  where cnt' = cnt { thmC = thmC cnt + 1 }

newExer :: Counter -> ([Int], Counter)
newExer cnt = ([chC cnt', exerC cnt'] , cnt')
  where cnt' = cnt { exerC = exerC cnt + 1 }

------

data Rose a = RNode a [Rose a]
type TOCItem = ( [Int]   -- section number
               , Text    -- title
               , Text    -- label
               )
type TOC = Rose TOCItem

------

type LblMap = Map Text [Int]
