{-# LANGUAGE OverloadedStrings #-}
module Html.Pure where

import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Development.Shake.FilePath ((</>))

import qualified Data.Text.Lazy as L
import Text.Replace

import Config
import Html.Types
import Html.RenderMonad

mkPage :: PRTOC
       -> (Maybe (RMonad ()), RMonad ())          -- (title, body)
       -> (Maybe (RMonad ()), Maybe (RMonad ()))  -- (prev, next)
       -> RMonad ()
mkPage toc (title, body) (prev, next) = do
  liftIO (T.readFile htmlHeader) >>= replacePath >>= putStrTR
  mkSideMenu toc
  putCharR '\n'
  mkMain (title, body) (prev, next)
  liftIO (T.readFile htmlFooter) >>= replacePath >>= putStrTR
 where htmlHeader = tmpls </> "html_pure" </> "pure_header.html"
       htmlFooter = tmpls </> "html_pure" </> "pure_footer.html"

mkSideMenu :: PRTOC -> RMonad ()
mkSideMenu toc =
  mkTagAttrsC "div" ([], ["menu"], []) .
    mkTagAttrsC "div" (["pure-menu"], [], []) $
     do menuHeader
        menuToCF
        menuPreface
        menuToC
        menuIx
        menuBiblio
        menuFooter
 where menuHeader = do
         mkTagAttrsC "h1" (["pure-menu-heading"], [], [])
            (putStrTR "函數程設與推論")
         mkTagAttrsC "h2" (["pure-menu-heading"], [], [])
            (putStrR "Functional Program Construction and Reasoning")
       menuToCF = do
         this <- reader thisFileR
         let selected = if this == ToC then ["pure-menu-selected"] else []
         mkTagAttrsC "p" (selected, [], [])
           (mkTagAttrsC "a" ([], [], [("href", pack (relPathToFile this ToC))])
            (putStrTR "目錄"))
       menuPreface = do
         this <- reader thisFileR
         let selected = if this == Preface then ["pure-menu-selected"] else []
         mkTagAttrsC "p" (selected, [], [])
           (mkTagAttrsC "a" ([], [], [("href", pack (relPathToFile this Preface))])
             (putStrTR "前言"))
       menuToC = do
         mkTagAttrsC "nav" (["nav"], [], [("role", "navigation")])
            (renderTOCsMenu 1 False toc)
       menuIx = do
         this <- reader thisFileR
         let selected = if this == Ix then ["pure-menu-selected"] else []
         mkTagAttrsC "p" (selected, [], [])
           (mkTagAttrsC "a" ([], [], [("href", pack (relPathToFile this Ix))])
             (putStrTR "索引"))
       menuBiblio = do
         this <- reader thisFileR
         let selected = if this == Biblio then ["pure-menu-selected"] else []
         mkTagAttrsC "p" (selected, [], [])
           (mkTagAttrsC "a" ([], [], [("href", pack (relPathToFile this Biblio))])
             (putStrTR "參考書目"))
       menuFooter = do
          mkTagAttrsC "p" (["author-info"], [], [])
            (do mkTagAttrsC "a" ([], [], [("href", "https://homepage.iis.sinica.edu.tw/pages/scm/")])
                  (putStrTR "穆信成 Shin-Cheng Mu")
                mkSCTag "br"
                putStrTR "中央研究院 資訊科學研究所")

mkMain :: (Maybe (RMonad ()), RMonad ())
       -> (Maybe (RMonad ()), Maybe (RMonad ()))
       -> RMonad ()
mkMain (title, body) (prev, next) =
  mkTagAttrsC "div" ([],["main"],[]) $ do
    maybe (return ())
      (mkTagAttrsC "div" (["header"],[],[]))
      title
    mkNavi (prev, next)
    mkTagAttrsC "div" (["content"],[],[]) body
    mkNavi (prev, next)


mkNavi :: (Maybe (RMonad ()), Maybe (RMonad ())) -> RMonad ()
mkNavi (prev, next) =
  mkTagAttrsC "div" (["navi"], [], [])
    $ do mapM_ (\prev -> mkTagAttrsC "div" (["previous"], [], [])
                        (do {putStrTR "&laquo;" ; prev})) prev
         mapM_ (\next -> mkTagAttrsC "div" (["next"], [], [])
                        (do {next; putStrTR "&raquo;"})) next

mkBox :: Text -> Text ->
         ([Text], [Text], [(Text, Text)]) -> RMonad () -> RMonad () -> RMonad ()
mkBox boxclass boxtitleclass (cs,ids,avs) title body =
  mkTagAttrsC "div" (boxclass:cs, ids, avs)
       (do mkTagAttrsC "h5" ([boxtitleclass], [], []) title
           body)

mkThmBox :: ([Text], [Text], [(Text, Text)]) -> RMonad () -> RMonad () -> RMonad ()
mkThmBox = mkBox "theorem" "theorem-title"

mkInfoBox :: ([Text], [Text], [(Text, Text)]) -> RMonad () -> RMonad () -> RMonad ()
mkInfoBox = mkBox "infobox" "infobox-title"

mkExerBox :: ([Text], [Text], [(Text, Text)]) -> RMonad () -> RMonad () -> RMonad ()
mkExerBox = mkBox "exercise" "exercise-title"

mkAnsBox :: ([Text], [Text], [(Text, Text)])
         -> [Int] -> RMonad () -> RMonad () -> RMonad ()
mkAnsBox (cs,ids,avs) exNum title body =
    mkTagAttrsC "div" (["wrap-collabsible"], [], [])
      (do mkSCTagAttrsC "input"
               (["toggle"], [ansLabel'], [("type", "checkbox")])
          mkTagAttrsC "label"
               (["lbl-toggle"], [], [("for", ansLabel')])
               (putStrTR "顯示答案")
          mkTagAttrsC "div" (["collapsible-content"], [], [])
            (mkTag "h5" title >> body)
          )
  where ansLabel = "ans-" ++ showNums exNum
        ansLabel' = pack ansLabel

mkFootnote :: [Int] -> Int -> RMonad () -> RMonad ()
mkFootnote chs i note =
  do mkSCTagAttrsC "input"
          (["toggle"], [fnoteLabel'], [("type", "checkbox")])
     mkTagAttrsC "label"
         (["fnote-toggle"], [], [("for", fnoteLabel')])
         (putStrR ("(註"++ show i ++ ") "))
     mkTagAttrsC "span" (["collapsible-footnote"], [], []) note
 where fnoteLabel = "footnote-" ++ showNums (chs++[i])
       fnoteLabel' = pack fnoteLabel

showNums [] = []
showNums [x] = show x
showNums (x:xs) = show x ++ "-" ++ showNums xs

renderTOCsMenu :: Int -> Bool -> PRTOC -> RMonad ()
renderTOCsMenu i _ [] = return ()
renderTOCsMenu i c ts =
   mkTagAttrsC "ul" (["pure-menu-list", levelCls] ++ collapse, [], [])
     (mapM_ (renderTOCMenu i) ts)
  where levelCls = pack ("menu-level-"++show i)
        collapse | c = ["collapsible-menu"]
                 | otherwise = []

renderTOCMenu i (RNode ((url, nums), title, lbl) []) = do
  this <- reader thisFileR
  let selected | this == Chap nums = ["pure-menu-selected"]
               | otherwise         = []
  mkTagAttrsC "li" (selected ++ ["pure-menu-item"], [], [])
   (mkTagAttrsC "a" ([],[],[("href", url)]) title)
  putCharR '\n'
renderTOCMenu i (RNode ((url, nums), title, lbl) ts) = do
  this <- reader thisFileR
  let (checked, selected)
        | this == Chap nums  = ([("checked","")], ["pure-menu-selected"])
        | nums `prefix` this = ([("checked","")], [])
        | otherwise          = ([],[])
  mkTagAttrsC "li" (selected ++ ["pure-menu-item"], [], [])
   (do mkTagAttrsC "a" ([],[],[("href", url)]) title
       mkSCTagAttrsC "input"
          (["toggle"], [liLabel'], checked++[("type", "checkbox")])
       mkTagAttrsC "label" (["menu-toggle"],[],[("for", liLabel')])(return ())
       renderTOCsMenu (i+1) True ts)
  putCharR '\n'
 where liLabel = "menu-li-" ++ showNums nums
       liLabel' = pack liLabel
       showNums [] = []
       showNums [x] = show x
       showNums (x:xs) = show x ++ "-" ++ showNums xs
       [] `prefix` (Chap _)          = True
       (x:xs) `prefix` (Chap [])     = False
       (x:xs) `prefix` (Chap (y:ys)) | x == y = xs `prefix` Chap ys
       (x:xs) `prefix` _             = False

replacePath :: Text -> RMonad Text
replacePath txt = do
  this <- reader thisFileR
  return (L.toStrict . replaceWithList (sysFileLoc this) . L.fromStrict $ txt)
 where inRoot     = [Replace "$CSSPATH" "css", Replace "$JSPATH" "js"]
       inChapters = [Replace "$CSSPATH" "../css", Replace "$JSPATH" "../js"]
       sysFileLoc (Chap _) = inChapters
       sysFileLoc _        = inRoot
