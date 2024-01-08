module Html.Counter where
import Data.Text (Text)
import Data.Map (Map)

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

initCounter :: Int -> Counter
initCounter i = Counter i 0 0 0 0 0 0 0

newChap :: Counter -> (RefNum, Counter)
newChap cnt = (([ch],[ch]), Counter ch 0 0 0 0 0 0 0)
     where ch = chC cnt + 1

newSec :: Counter -> (RefNum, Counter)
newSec cnt = (([ch, sec], [ch, sec]), cnt')
  where cnt' = cnt { secC = sec, subsecC = 0}
        (ch, sec) = (chC cnt, secC cnt + 1)

newSubSec :: Counter -> (RefNum, Counter)
newSubSec cnt = (([ch, sec], [ch, sec, subsec]), cnt')
  where cnt' = cnt { subsecC = subsec }
        (ch, sec, subsec) = (chC cnt, secC cnt, subsecC cnt + 1)

newHeader :: Int -> Counter -> (RefNum, Counter)
newHeader 1 = newChap
newHeader 2 = newSec
newHeader 3 = newSubSec

newThm :: Counter -> (RefNum, Counter)
newThm cnt = (([ch, sec], [ch, thmC cnt']), cnt')
  where cnt' = cnt { thmC = thmC cnt + 1 }
        (ch, sec) = (chC cnt, secC cnt)

newExer :: Counter -> (RefNum, Counter)
newExer cnt = (([ch, sec], [ch, exerC cnt']), cnt')
  where cnt' = cnt { exerC = exerC cnt + 1 }
        (ch, sec) = (chC cnt, secC cnt)

newFNote :: Counter -> (RefNum, Counter)
newFNote cnt = (([ch, sec], [fnoteC cnt']), cnt')
  where cnt' = cnt { fnoteC = fnoteC cnt + 1 }
        (ch, sec) = (chC cnt, secC cnt)

newEq :: Counter -> (RefNum, Counter)
newEq cnt = (([ch, sec], [ch, eqC cnt']), cnt')
  where cnt' = cnt { eqC = eqC cnt + 1 }
        (ch, sec) = (chC cnt, secC cnt)

------

data Rose a = RNode a [Rose a]
type TOCItem = ( [Int]   -- section number
               , Text    -- title
               , Text    -- label
               )
type TOC = Rose TOCItem

------

type LblMap = Map Text RefNum
