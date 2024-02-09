module Text.BibTeX.Entry where

import Control.Arrow (first)
import Data.Char (toLower)


data T =
   Cons {
      entryType :: String,
      identifier :: String,
      fields :: [(String, String)],
      author :: [AuthorName]
   }
   deriving (Show)

type AuthorName = (String, String) -- Surname, Othernames

{- |
Convert the name style \"Surname, First name\" into \"First name Surname\".
-}
flipName :: String -> String
flipName name =
   let (surname, firstName) = break (','==) name
   in  dropWhile (flip elem (", " :: String)) firstName ++ " " ++ surname

lowerCaseFieldNames :: T -> T
lowerCaseFieldNames entry =
   entry {fields = map (first (map toLower)) $ fields entry}
