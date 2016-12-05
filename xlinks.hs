module XLinks where

import Html
import Pages
import Util

data Cfg = Cfg { lis :: Util.KeyVals, css :: String, cssSel :: String } deriving (Read)

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ _ page = map per_link (lis cfg) where
        per_link li =
            Html.out "li" [("class",(css cfg)++(when isSel (" "++(cssSel cfg)) ""))] [link] where
                link = Html.T "a" [("",txt),("href",fn++".html")] []
                isSel = fn==fname ; fn = fst li ; txt = snd li
        fname = Util.fnName $ Pages.fname page
