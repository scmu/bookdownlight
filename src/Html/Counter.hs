module Html.Counter where

import Data.Text (Text)
import Data.Map (Map)

import Cheapskate (Inlines)

import Html.Types

initChCounter :: Int -> Counter
initChCounter i = Counter i 0 0 0 0 0 0 0 0

incChap :: Counter -> (RefNum, Counter)
incChap cnt = (([ch],[ch]), cnt { chC = ch })
  where ch = chC cnt + 1

newChap :: Counter -> (RefNum, Counter)
newChap cnt = (([ch],[ch]), Counter ch 0 0 0 0 0 0 0 0)
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

newIdx :: Counter -> (RefNum, Counter)
newIdx cnt = (([ch, sec], [ixC cnt']), cnt')
  where cnt' = cnt { ixC = ixC cnt + 1 }
        (ch, sec) = (chC cnt, secC cnt)

buildRose :: Int -> [(Int, a)] -> [Rose a]
buildRose i = fst . parseRose i

parseRose :: Int -> [(Int, a)] -> ([Rose a], [(Int, a)])
parseRose i [] = ([], [])
parseRose i ((j,x):xs)
  | i == j = let (ts, ys) = parseRose (i+1) xs
                 (us, zs) = parseRose i ys
             in (RNode x ts : us, zs)
  | i > j  = ([], (j,x):xs)
  | i < j  = parseRose j ((j,x):xs)

buildAdjList :: Int -> [(Int, a)] -> [[(Maybe a, Maybe a)]]
buildAdjList l =
     breakByChap . bdAdj . filter ((l >=) . fst)
 where
  bdAdj :: [(a, b)] -> [(a, (Maybe b, Maybe b))]
  bdAdj [] = []
  bdAdj xs = zip xs1 (zip (Nothing : map Just xs2) (map Just (tail xs2) ++ [Nothing]))
    where (xs1, xs2) = unzip xs
  breakByChap :: [(Int, b)] -> [[b]]
  breakByChap [] = []
  breakByChap xs | null xs1  = breakByChap' xs2
                 | otherwise = map snd xs1 : breakByChap' xs2
    where (xs1, xs2) = break ((1==) . fst) xs
  breakByChap' [] = [] -- inv: input starts with ((1,_):_)
  breakByChap' ((_,x): xs) = (x:map snd xs1) : breakByChap' xs2
    where (xs1, xs2) = break ((1==) . fst) xs
