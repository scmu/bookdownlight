{-# LANGUAGE OverloadedStrings #-}
module Html.Pure where

import Control.Arrow ((***))
import Control.Monad.State
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Development.Shake.FilePath ((</>))

import Html.RenderMonad

mkPage :: String -> (Maybe (RMonad ()), RMonad ()) -> RMonad ()
mkPage tmpls (title, body) = do
  liftIO (T.readFile htmlHeader) >>= putStrTR
  mkMain (title, body)
  liftIO (T.readFile htmlFooter) >>= putStrTR
 where htmlHeader = tmpls </> "html_pure" </> "pure_header.html"
       htmlFooter = tmpls </> "html_pure" </> "pure_footer.html"


mkMain :: (Maybe (RMonad ()), RMonad ()) -> RMonad ()
mkMain (title, body) =
  mkTagAttrsC "div" ([],["main"],[])(do
    maybe (return ())
      (mkTagAttrsC "div" (["header"],[],[]))
      title
    mkTagAttrsC "div" (["content"],[],[]) body)

-- mkBreadCrumb :: RMonad ()
-- mkBreadCrumb = putStrTR
--    "<ol class=\"breadcrumb mb-4\"><li class=\"breadcrumb-item\"><a href=\"index.html\">Dashboard</a></li><li class=\"breadcrumb-item active\">Sidenav Light</li></ol>"

-- mkFooter :: RMonad ()
-- mkFooter = putStrTR
--   "<footer class=\"py-4 bg-light mt-auto\">\
--   \<div class=\"container-fluid px-4\">\
--   \<div class=\"d-flex align-items-center justify-content-between small\">\
--       \<div class=\"text-muted\">Copyright &copy; Your Website 2023</div>\
--       \<div>\
--           \<a href=\"#\">Privacy Policy</a>\
--           \&middot;\
--           \<a href=\"#\">Terms &amp; Conditions</a>\
--       \</div>\
--   \</div>\
--   \</div></footer>"
----

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
