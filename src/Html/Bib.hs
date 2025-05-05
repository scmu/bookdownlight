{-# LANGUAGE OverloadedStrings #-}
module Html.Bib where

import System.IO (readFile)

import qualified Data.Text as Text
import qualified Text.BibTeX.Entry as E
import qualified Text.BibTeX.Parse as P
import Text.Parsec

import Control.Monad.Reader
import qualified Data.Map as Map

import Config
import Html.Types
import Html.RenderMonad

parseBib :: FilePath -> IO [E.T]
parseBib fname = do
  cnt <- readFile fname
  let res = parse P.file fname cnt
  case res of
    Left err -> error (show err)
    Right entries -> return entries

renderBib :: [E.T] -> RMonad ()
renderBib [] = return ()
renderBib bib = mkTagAttrsC "ul" (["biblio"], [], []) (mapM_ renderEntry bib)

renderEntry :: E.T -> RMonad ()
renderEntry entry@(E.Cons _ idn _ _) =
  mkTagAttrsC "li" ([], [Text.pack idn], [])
    (runEF renderEntryByType entry)

renderEntryByType :: EntryFilter (RMonad ())
renderEntryByType = entryTypeSwitch
  (EF (putStrR . show))
  [ ("Book"          , renderBook)
  , ("Article"       , renderArticle)
  , ("InProceedings" , renderInProceedings)
  , ("InCollection"  , renderInCollection)
  , ("InBook"        , renderInBook)
  , ("Misc"          , renderMisc)]

renderBook :: EntryFilter (RMonad ())
renderBook =
  renderAuthors <> lit ". " <>
  selSolBook <> lit ". " <>
  renderPublisher <>
  renderDate <> lit "." <> renderNote

renderArticle :: EntryFilter (RMonad ())
renderArticle =
  renderAuthors <> lit ". " <>
  selTitle <> lit ". " <>
  selBooktitle "journal" <> lit ", " <>
  selP "volume" <>
  renderNumber <> renderPages <> lit ", " <>
  renderDate <> lit "." <> renderNote
 where renderNumber = ifPresentR "number"
          (\num -> putCharR '(' >> putStrR num >> putCharR ')')
       renderPages = ifPresentR "pages"
          (\ps -> putCharR ':' >> putStrR ps)

renderInProceedings :: EntryFilter (RMonad ())
renderInProceedings =
  renderAuthors <> lit ". " <>
  selTitle <> lit ". " <>
  renderProcInfo  <> lit ", " <>
  renderPages <> renderPublisher <> renderDate <> lit "." <> renderNote

renderInCollection :: EntryFilter (RMonad ())
renderInCollection =
  renderAuthors <> lit ". " <>
  selTitle <> lit ". " <>
  renderProcInfo <> lit ", " <>
  renderPages <> renderPublisher <> renderDate <> lit "." <> renderNote

renderInBook :: EntryFilter (RMonad ())
renderInBook =
  renderAuthors <> lit ". " <>
  selSolBook <> lit ", " <>
  renderChapter <>
  renderPages <> renderPublisher <> renderDate <> lit "." <>
  renderNote
 where renderChapter = ifPresentR "chapter"
          (\chap -> putStrTR "Chapter " >> putStrR chap >> putStrTR ", ")

renderMisc :: EntryFilter (RMonad ())
renderMisc =
  renderAuthors <> lit ". " <>
  selTitle <> lit ". " <>
  renderHowPublished <>
  renderInstitution <> renderPublisher <>
  renderDate <> lit "." <>
  renderNote

--

renderProcInfo :: EntryFilter (RMonad ())
renderProcInfo =
  lit "In " <> renderEditors <>
  selBooktitle "booktitle" <>
  renderSeries
 where
  renderEditors = ifPresentR "editor"
          (\editors -> putStrR editors >> putStrTR ", editors, ")
  renderSeries = ifPresentElse "series"
    (\ser -> lit ", " <> lit ser <>
             ifPresentR "number"
               (\num -> putStrTR " no. " >> putStrR num))
    mempty

renderAuthors :: EntryFilter (RMonad ())
renderAuthors = printAuthorList <$> selParsedAuthors

renderPages = ifPresentR "pages"
    (\ps -> putStrTR "pages " >> putStrR ps >> putStrTR ". ")

renderPublisher = ifPresentR "publisher"
      (\pub -> putStrR pub >> putStrTR ", ")

renderInstitution = ifPresentR "institution"
            (\pub -> putStrR pub >> putStrTR ", ")

renderNote = ifPresentR "note"
      (\note -> putCharR ' ' >> putStrR note)

renderDate = ifPresentR "month"
              (\note -> putStrR note >> putCharR ' ') <>
             selP "year"

renderHowPublished = ifPresentR "howpublished"
    (\how -> putCharR ' ' >> putStrR how >> putStrTR ", ")


selTitle :: EntryFilter (RMonad ())
selTitle = mkTagAttrsC "span" (["title"], [], [] ) <$> selP "title"

selBooktitle :: String -> EntryFilter (RMonad ())
selBooktitle booktitle = mkTagAttrsC "span" (["book"], [], [] ) <$> selP booktitle

selSolBook :: EntryFilter (RMonad ())
selSolBook =  mkTagAttrsC "span" (["book", "title"], [], [] )<$> selP "title"

selParsedAuthors :: EntryFilter [E.AuthorName]
selParsedAuthors = EF E.author


printAuthorList :: [E.AuthorName] -> RMonad ()
printAuthorList [] = return ()
printAuthorList [n] = printAuthorName n
printAuthorList [n1,n2] = printAuthorName n1 >> putStrTR " and " >>
                          printAuthorName n2
printAuthorList (n:ns) = printAuthorName n >> printAuthorList3 ns
  where printAuthorList3 [] = return () -- shouldn't happen
        printAuthorList3 [n] = putStrTR ", and " >> printAuthorName n
        printAuthorList3 (n:ns) = putStrTR ", " >> printAuthorName n >>
                                  printAuthorList3 ns

printAuthorName :: E.AuthorName -> RMonad ()
printAuthorName (sur, other) = putStrR other >> putCharR ' ' >> putStrR sur

--
newtype EntryFilter a = EF { runEF :: E.T -> a }

instance Semigroup a => Semigroup (EntryFilter a) where
  (EF f) <> (EF g) = EF $ \e -> f e <> g e

instance Monoid a => Monoid (EntryFilter a) where
  mempty = EF (const mempty)

instance Functor EntryFilter where
  fmap f (EF g) = EF (f . g)

--

lit :: String -> EntryFilter (RMonad ())
lit x = EF (const (putStrR x))

select :: String -> EntryFilter String
select attr = EF (maybe "" id . lookup attr . E.fields)

selP :: String -> EntryFilter (RMonad ())
selP attr = putStrR <$> select attr

selIdentifier = EF (E.identifier)

entryTypeSwitch :: EntryFilter a -> [(String, EntryFilter a)]
                -> EntryFilter a
entryTypeSwitch def cases = EF $ \entry ->
  case lookup (E.entryType entry) cases of
    Just action -> runEF action entry
    Nothing     -> runEF def entry

ifPresentElse :: String -> (String -> EntryFilter a)
    -> EntryFilter a
    -> EntryFilter a
ifPresentElse attr action otherwz = EF $ \entry ->
  case lookup attr (E.fields entry) of
    Just val -> runEF (action val) entry
    Nothing  -> runEF otherwz entry

ifPresentR :: Monoid a =>
      String -> (String -> RMonad a) -> EntryFilter (RMonad a)
ifPresentR = ifPresent

ifPresent :: (Monad m, Monoid a) =>
      String -> (String -> m a) -> EntryFilter (m a)
ifPresent attr action =
  ifPresentElse attr (\val -> EF . const $ action val)
       (EF (const (return mempty)))

-- Citations

renderCiteT :: Text.Text -> Maybe Text.Text -> RMonad ()
renderCiteT idn opt = maybe err rCiteT =<< lookupBib idn
  where err = putStrTR "[{" >> putStrTR idn >> putStrTR "} not found]"
        rCiteT entry = do
          this <- reader thisFileR
          mkTagAttrsC "a" ([],[],[("href", href this)])
           (renderCAuthors (E.author entry) >> putStrTR " [" >>
            putStrR (runEF (select "year") entry) >>
            showOpt opt >>
            putCharR ']')
        showOpt Nothing = return ()
        showOpt (Just txt) = putStrTR ", " >> putStrTR txt
        href this = showLongHRef_ this Biblio idn

renderCiteP1 :: Text.Text -> Maybe Text.Text -> RMonad ()
renderCiteP1 idn opt = maybe err rCiteP =<< lookupBib idn
  where err = putStrTR "[{" >> putStrTR idn >> putStrTR "} not found]"
        rCiteP entry = do
          this <- reader thisFileR
          putCharR ' '
          mkTagAttrsC "a" ([],[],[("href", href this)])
           (putStrTR "[" >> renderCAuthors (E.author entry) >> putCharR ' ' >>
            putStrR (runEF (select "year") entry) >>
            showOpt opt >>
            putCharR ']')
        showOpt Nothing = return ()
        showOpt (Just txt) = putStrTR ", " >> putStrTR txt
        href this = showLongHRef_ this Biblio idn

renderCitePs :: [(Text.Text, Maybe Text.Text)] -> RMonad ()
renderCitePs [] = return ()
renderCitePs cites = putCharR '[' >> rCitePs cites >> putCharR ']'
 where rCitePs [] = return ()
       rCitePs [(idn,_)] = maybe (err idn) (rCite1 idn) =<< lookupBib idn
       rCitePs ((idn,_):rest) =
          (maybe (err idn) (rCite1 idn) =<< lookupBib idn) >>
          putStrTR ", " >> rCitePs rest
       err idn = putStrTR "[{" >> putStrTR idn >> putStrTR "} not found]"
       rCite1 idn entry = do
          this <- reader thisFileR
          mkTagAttrsC "a" ([],[],[("href", href this)])
            (renderCAuthors (E.author entry) >> putCharR ' ' >>
             putStrR (runEF (select "year") entry))
         where href this = showLongHRef_ this Biblio idn

renderCAuthors [] = return ()
renderCAuthors [(sur,_)] = putStrR sur
renderCAuthors [(sur1,_), (sur2,_)] =
  putStrR sur1 >> putStrTR " and " >> putStrR sur2
renderCAuthors ((sur,_):_) = putStrR sur >> putStrTR " et al."

showLongHRef_ :: FileRole -> FileRole -> Text.Text -> Text.Text
showLongHRef_ this fr lbl =
   Text.append (Text.pack (fname ++ "#")) lbl
  where fname = relPathToFile this fr
