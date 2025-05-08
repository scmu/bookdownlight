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
htmlBase  = root </> "docs"
lhsChs    = lhsBase  </> "Chapters"
texChs    = texBase  </> "Chapters"
htmlChs   = htmlBase </> "Chapters"

tmpls     = root </> "templates"
tmp       = root </> "tmp"

data FileRole = Index | Preface | ToC | Chap [Int] | Ix | Biblio
  deriving Eq

fileName :: FileRole -> String
fileName Index        = "index"
fileName Preface      = "Preface"
fileName ToC          = "ToC"
fileName (Chap (c:_)) = chapters !! c
fileName Ix           = "Ix"
fileName Biblio       = "Biblio"

mdNamePath :: FileRole -> String
mdNamePath Preface      = contents </> fileName Preface <.> "md"
mdNamePath (Chap (c:_)) = contents </> (chapters !! c) <.> "md"

-- Html Specific

htmlName :: FileRole -> String
htmlName fr = fileName fr <.> "html"

htmlNamePath :: FileRole -> String
htmlNamePath fr@(Chap c) = htmlChs  </> fileName fr <.> "html"
htmlNamePath fr          = htmlBase </> fileName fr <.> "html"

hauxNamePath :: Int -> String
hauxNamePath c = tmp </> "html" </> chapters !! c <.> "haux"

relPathToFile :: FileRole -> FileRole -> String
relPathToFile (Chap _) tar@(Chap _) = htmlName tar
relPathToFile (Chap _) tar          = ".." </> htmlName tar
relPathToFile _        tar@(Chap _) = "Chapters" </> htmlName tar
relPathToFile _        tar          = htmlName tar
