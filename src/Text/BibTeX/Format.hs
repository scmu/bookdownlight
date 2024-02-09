module Text.BibTeX.Format
     (entry, enumerate, authorList, commaSepList, sepList)
   where

import qualified Text.BibTeX.Entry as Entry

import Control.Arrow (first)
import Data.List (intersperse)

entry :: Entry.T -> String
entry (Entry.Cons entryType bibId items _) =
   let formatItem (name, value) =
         "  "++name++" = {"++value++"},\n"
   in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
       concatMap formatItem items ++
       "}\n"

enumerate :: [String] -> String
enumerate =
   switchR "" $ \xs0 lastWord0 ->
   flip (switchR lastWord0) xs0 $ \xs1 lastWord1 ->
   foldr
      (\word -> (word ++) . (", " ++))
      (lastWord1 ++ " and " ++ lastWord0) xs1

authorList :: [String] -> String
authorList =
   concat . intersperse " and "

commaSepList :: [String] -> String
commaSepList = sepList ','

sepList :: Char -> [String] -> String
sepList sep = concat . intersperse (sep:" ")

--

{- |
Should be prefered to 'init' and 'last'.

prop> \xs -> switchR True (\ixs lxs -> ixs == init xs && lxs == last xs) (xs::String)
-}
{-# INLINE switchR #-}
switchR :: b -> ([a] -> a -> b) -> [a] -> b
switchR n j =
   maybe n (uncurry j) . viewR

{- |
Should be prefered to 'init' and 'last'.

prop> \xs -> maybe True ((init xs, last xs) == ) (viewR (xs::String))
-}
viewR :: [a] -> Maybe ([a], a)
viewR =
   foldr (\x -> Just . forcePair . maybe ([],x) (first (x:))) Nothing

{-# INLINE forcePair #-}
forcePair :: (a,b) -> (a,b)
forcePair ~(a,b) = (a,b)
