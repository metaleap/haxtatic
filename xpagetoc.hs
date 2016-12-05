module XPageToc where

import Html
import Pages


data Cfg = Cfg { outerTag :: String } deriving (Read)


ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ _ page = map per_heading (tail $ Pages.titles page) where
        per_heading h2 = Html.emit t where
            lnk = Html.T "a" [("",h2),("href","#"++h2)] []
            t = if (Html.noTag outertag) then lnk else outertag { Html.tagchildren = [lnk] }
    outertag = Html.T (outerTag cfg) [] []
