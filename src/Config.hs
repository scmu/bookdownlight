module Config where

import Development.Shake.FilePath

-- configuration info.

chapters :: [String]
chapters = [ "Introduction"
           , "Basics"
           , "Induction"
           , "SearchTrees"
           , "Semantics"
           , "Derivation"
           , "Folds"
           , "SegProblems"
           , "Monads"
           ]

numOfChapters :: Int
numOfChapters = length chapters

-- paths

root      = "fpcr"
contents  = root </> "contents"

texBase   = root </> "tex"
lhsBase   = root </> "lhs"
htmlBase  = root </> "html"
lhsChs    = lhsBase  </> "Chapters"
texChs    = texBase  </> "Chapters"
htmlChs   = htmlBase </> "Chapters"

tmpls     = root </> "templates"
tmp       = root </> "tmp"

mdNamePath :: Int -> String
mdNamePath i = contents </> (chapters !! i) <.> "md"

-- Html Specific

data FileRole = Preface | ToC | Chap [Int] | Ix | Biblio
  deriving Eq

fileName :: FileRole -> String
fileName Preface      = "Preface"
fileName ToC          = "ToC"
fileName (Chap (c:_)) = chapters !! c
fileName Ix           = "Ix"
fileName Biblio       = "Biblio"

htmlName :: FileRole -> String
htmlName fr = fileName fr <.> "html"

htmlNamePath :: FileRole -> String
htmlNamePath fr = htmlChs </> fileName fr <.> "html"

hauxNamePath :: Int -> String
hauxNamePath c = tmp </> "html" </> chapters !! c <.> "haux"
