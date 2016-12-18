module XPageToc where

import qualified Html
import qualified Pages
import qualified Util


data Cfg = Cfg { outerTag :: String, emptyIf :: Int, emptyMarkup :: String, linkAtts :: Util.KeyVals } deriving (Read)


ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ _ page = if (length headings)<=(emptyIf cfg) then [emptyMarkup cfg] else map per_heading headings where
        headings = tail $ Pages.titles page
        per_heading h2 = Html.emit t where
            t = if (Html.noTag outertag) then lnk else outertag { Html.sub = [lnk] }
            lnk = Html.T "a" ([("",h2),("href","#"++(Pages.h2innerToId h2))]++(linkAtts cfg)) []
    outertag = Html.T (outerTag cfg) [] []
