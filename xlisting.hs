module XListing where

import Data.List

import Html
import Pages
import Posts
import Util


data Args = Args { cats :: [String], years :: (String,String), subcats :: [String], subcatsonly :: Bool, groups :: Bool, linkers :: Util.KeyVals } deriving (Read)
data Cfg = Cfg {
        lean :: Bool, cssCat :: String, cssTitle :: String, cssDesc :: String, cssUl :: String,
        groupTag :: String, imgPaths :: Util.KeyVals
    } deriving (Read)


picSrc cfg ppic =
    let (pph:ppt) = Util.splitBy '>' ppic in
        if ppt==[] then pph else let imgpathprefix = Util.keyVal (imgPaths cfg) pph "" "" in
            if null imgpathprefix then pph else imgpathprefix ++ head ppt


buildLi_Full cfg args post = let
    csscat = cssCat cfg ; cssdesc = cssDesc cfg ; csstitle = cssTitle cfg
    pid = Util.join "_" $ Posts.fn post; pcat = cat post ; psubcat = subcat post ; ptitle = title post ; ptext = text post
    nocats = (not (Util.is csscat)) || ((null psubcat) && ((null pcat) || subcatonly)) ; subcatonly = subcatsonly args
    linkhref = Util.keyVal (linkers args) pcat (Posts.link post) ("#"++pid)
    ppic = pic post ; picsrc = picSrc cfg ppic
    in Html.T "li" [("name",pid),("id",pid)] [
        Html.T (if null linkhref then "" else "a") [("href",linkhref)] [
                Html.T (when nocats "" "div") [("",(if subcatonly then "" else ""++pcat++": ")++psubcat),("class",csscat)] [],
                Html.T "h3" [("",ptitle),("class",csstitle)] $ if null ppic then [] else [Html.T "img" [("src",picsrc),("title",ptitle)] []],
                Html.T "div" [("",ptext),("class",cssdesc)] []
            ]
        ]

buildLi_Lean cfg args post =
    Html.T "li" [] [
            Html.T "span" [("",subcat post)] [],
            Html.T "a" [("",title post), ("href",linkhref)] []
        ] where
        linkhref = Util.keyVal (linkers args) (cat post) (Posts.link post) ("#"++pid)
        pid = Util.join "_" $ Posts.fn post


ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr page = fulloutput where
        fulloutput = if nogroups then [Html.emit $ tagUl itemsall] else concat $ map persection sectiontitles
        tagUl = Html.T "ul" [("class",cssUl cfg)]
        sectiontitles = Data.List.nub $ map (head . Posts.fn) (Pages.allPosts page)
        persection year = let items = itemsfor year in if items==[] then [] else
            [Html.out grouptag [("", year)] [], Html.emit $ tagUl items]

        ispostvisible yearmin yearmax post = let pcat = cat post ; psubcat = subcat post ; pyear = head $ Posts.fn post in
            (allcats || elem pcat pcats) && (allsubcats || elem psubcat psubcats) && pyear >= yearmin && pyear <= yearmax
        pcats = cats args; pyears = years args; ymin = fst pyears; ymax = snd pyears ; allcats = pcats==[]
        psubcats = subcats args ; allsubcats = psubcats==[]
        args = read ("Args {"++argstr++"}") :: Args ; nogroups = null grouptag || not (groups args)
        grouptag = groupTag cfg

        itemsall = map doitem $ filter (ispostvisible ymin ymax) $ Pages.allPosts page
        itemsfor year = map doitem $ filter (ispostvisible (max year ymin) (min year ymax)) $ Pages.allPosts page
        doitem post = (if (lean cfg) then buildLi_Lean else buildLi_Full) cfg args post
