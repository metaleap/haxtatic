module XLinks where

import qualified Html
import qualified Pages
import qualified Util

import qualified Data.Char

data Cfg = Cfg { lis :: Util.KeyVals, linkid :: String, css :: String, cssSel :: String } deriving (Read)

ext tagname cfg bnames = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ _ page = map per_link (lis cfg) where
        per_link li =
            Html.out "li" [("class",(css cfg)++(if isSel then (" "++(cssSel cfg)) else ""))] [link] where
                link = Html.T "a" [("",text),("href",href++".html"),("id", Util.replaceIn (linkid cfg) [("{%_k}", fst li),("{%_v}", snd li)])] []
                isSel = href==fname
                href = snd $ fallback $ fst li
                text = let fb = (fallback $ snd li) ; fb2 = snd fb in if fst fb then ((Data.Char.toUpper $ head fb2):(tail fb2)) else fb2
                fallback v = if null v then (True,(if null bnames || (elem fname bnames) then fname else head bnames)) else (False,v)
        fname = Util.fnName $ Pages.fname page
