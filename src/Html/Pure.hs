{-# LANGUAGE OverloadedStrings #-}
module Html.Pure where

import Control.Arrow ((***))
import Control.Monad.State
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Development.Shake.FilePath ((</>))

import Html.Types
import Html.RenderMonad

mkPage :: String -> PRTOC -> (Maybe (RMonad ()), RMonad ()) -> RMonad ()
mkPage tmpls toc (title, body) = do
  liftIO (T.readFile htmlHeader) >>= putStrTR
  mkSideMenu toc
  mkMain (title, body)
  liftIO (T.readFile htmlFooter) >>= putStrTR
 where htmlHeader = tmpls </> "html_pure" </> "pure_header.html"
       htmlFooter = tmpls </> "html_pure" </> "pure_footer.html"

mkSideMenu :: PRTOC -> RMonad ()
mkSideMenu toc =
  mkTagAttrsC "div" ([], ["menu"], []) .
    mkTagAttrsC "div" (["pure-menu"], [], []) $
     do mkTagAttrsC "h1" (["pure-menu-heading"], [], [])
           (putStrTR "函數程設與推論")
        mkTagAttrsC "nav" (["nav"], [], [("role", "navigation")])
           (renderTOCsMenu 1 False toc)

mkMain :: (Maybe (RMonad ()), RMonad ()) -> RMonad ()
mkMain (title, body) =
  mkTagAttrsC "div" ([],["main"],[])(do
    maybe (return ())
      (mkTagAttrsC "div" (["header"],[],[]))
      title
    mkTagAttrsC "div" (["content"],[],[]) body)

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
  mkTagAttrsC "li" (["pure-menu-item"], [], [])
   (mkTagAttrsC "a" ([],[],[("href", url)]) title)
renderTOCMenu i (RNode ((url, nums), title, lbl) ts) = do
  mkTagAttrsC "li" (["pure-menu-item"], [], [])
   (do mkTagAttrsC "a" ([],[],[("href", url)]) title
       mkSCTagAttrsC "input" (["toggle"], [liLabel'], [("type", "checkbox")])
       mkTagAttrsC "label" (["menu-toggle"],[],[("for", liLabel')])(return ())
       renderTOCsMenu (i+1) True ts)
 where liLabel = "menu-li-" ++ showNums nums
       liLabel' = pack liLabel
       showNums [] = []
       showNums [x] = show x
       showNums (x:xs) = show x ++ "-" ++ showNums xs
