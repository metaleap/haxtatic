module XImage where

import qualified Html
import qualified Pages


type Args = (String,String)
data Cfg = Cfg { link :: Bool, linkCss :: String, imgCss :: String, imgSrcPath :: String } deriving (Read)

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr _ = [ Html.emit $ if (link cfg) then lnk else img ] where
        img = Html.T "img" [("src",url),("title",title),("class",imgCss cfg)] []
        lnk = Html.T "a" [("href",url),("class",linkCss cfg)] [img]
        url = (imgSrcPath cfg)++(fst args) ; title = snd args ; args = read ("("++argstr++")") :: Args
