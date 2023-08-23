{-# LANGUAGE OverloadedStrings #-}

module Html.Dict where
import Data.Text (Text)

type DictState = (Text, [Int], Text, [Int])
dict :: [DictState]
