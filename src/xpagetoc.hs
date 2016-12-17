module XPageToc where

import qualified Html
import qualified Pages
import qualified Util


data Cfg = Cfg { outerTag :: String, emptyMarkup :: String, linkAtts :: Util.KeyVals } deriving (Read)


ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ _ page = if null headings then [emptyMarkup cfg] else map per_heading headings where
        headings = tail $ Pages.titles page
        per_heading h2 = Html.emit t where
            lnk = Html.T "a" ([("",h2),("href","#"++h2)]++(linkAtts cfg)) []
            t = if (Html.noTag outertag) then lnk else outertag { Html.sub = [lnk] }
    outertag = Html.T (outerTag cfg) [] []
