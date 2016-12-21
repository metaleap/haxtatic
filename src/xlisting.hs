{-# OPTIONS_GHC -Wall #-}
module XListing where

import qualified Html
import qualified Pages
import qualified Posts
import qualified Util

import qualified Data.List



data Args = Args {
        cats :: [String],
        years :: (String,String),
        subcats :: [String],
        subcatsonly :: Bool,
        groups :: Grouping,
        linkers :: Util.KeyVals,
        blogdf :: String }
    deriving (Read)

data Grouping =
        NoGrouping |
        ByYear |
        ByMonth |
        ByDay
    deriving (Read, Eq)

data Cfg = Cfg {
        lean :: Bool,
        cssCat :: String,
        cssTitle :: String,
        cssDesc :: String,
        cssUl :: String,
        groupTag :: String,
        imgPaths :: Util.KeyVals
    } deriving (Read)



picSrc::
    Cfg-> String->
    String
picSrc cfg ppic =
    let (pph:ppt) = Util.splitBy '>' ppic in
        if ppt==[] then pph else let imgpathprefix = Util.keyVal (imgPaths cfg) pph "" in
            if null imgpathprefix then pph else imgpathprefix ++ head ppt


buildLi_Full::
    Cfg-> Args-> Posts.Post-> String->
    Html.Tag
buildLi_Full cfg args post datecat = let
    csscat = cssCat cfg ; cssdesc = cssDesc cfg ; csstitle = cssTitle cfg
    pid = Util.join "_" $ Posts.fn post; pcat = Posts.cat post ; ptitle = Posts.title post ; ptext = Posts.text post
    psubcat = if null datecat then Posts.subcat post else datecat
    nocats = (null csscat) || ((null psubcat) && ((null pcat) || subcatonly)) ; subcatonly = subcatsonly args
    linkhref = Util.keyValApp (linkers args) pcat (Posts.link post) ("#"++pid)
    ppic = if null (Posts.pic post) then autoicon else Posts.pic post ; picsrc = picSrc cfg ppic
    autoicon = firstimg firstimginner where
        firstimginner = let
            l = filter Util.is $ map (Html.tagInner2 "img") $ lines (Posts.origraw post)
            in if null l then "" else Util.trimStart (head l)
        firstimg ('s':'r':'c':'=':'\"':src) = take (max 0 (Util.indexOf '\"' src)) src
        firstimg _ = ""
    in Html.T "li" [("name",pid),("id",pid)] [
        Html.T (if null linkhref then "" else "a") [("href",linkhref)] $
                (if nocats then [] else [ Html.T (if nocats then "" else "div") [("",(if subcatonly then "" else ""++pcat++": ")++psubcat),("class",csscat)] [] ]) ++
                [ Html.T "h3" [("",ptitle),("class",csstitle)] $ if null ppic then [] else [Html.T "img" [("src", if null picsrc then autoicon else picsrc),("class", if null autoicon then "" else "ml-feed-autoicon"),("title",ptitle)] []],
                Html.T "div" [("",ptext),("class",cssdesc)] [] ]
        ]

buildLi_Lean::
    Cfg-> Args-> Posts.Post-> String->
    Html.Tag
buildLi_Lean cfg args post datecat =
    Html.T "li" [] $
        (if (null $ cssCat cfg) then [] else [Html.T "span" [("", if null datecat then Posts.subcat post else datecat)] []])++
        [ Html.T "a" [("", Posts.title post), ("href",linkhref)] [] ]
        where
            linkhref = Util.keyValApp (linkers args) (Posts.cat post) (Posts.link post) ("#"++pid)
            pid = Util.join "_" $ Posts.fn post


ext::
    String-> Cfg->
    Pages.X
ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr page = fulloutput where
        fulloutput = if nogroups then [Html.emit $ tagUl itemsall] else concat $ map persection sections
        tagUl = Html.T "ul" [("class",cssUl cfg)]
        sections = Data.List.nub $ map (groupby grouping) (Pages.allPosts page)
        groupby ByYear x = (take 1 $ Posts.fn x)
        groupby ByMonth x = (take 2 $ Posts.fn x)
        groupby ByDay x = (take 3 $ Posts.fn x)
        groupby NoGrouping _ = [] -- by `nogroups` this shouldn't ever be reached
        persection section =
            let items = itemsfor section in
                if items==[] then [] else
                    [Html.out grouptag [("", formatdate section)] [], Html.emit $ tagUl items]

        ispostvisible yearmin yearmax month day post =
            let pcat = Posts.cat post ; psubcat = Posts.subcat post ; pfn = Posts.fn post
                pyear = Util.fnYear pfn ; pmonth = Util.fnMonth pfn ; pday = Util.fnDay pfn
                in (allcats || elem pcat pcats)
                    && (allsubcats || elem psubcat psubcats)
                        && pyear >= yearmin && pyear <= yearmax
                            && ((null month) || pmonth==month)
                                && ((null day) || pday == day)
        pcats = cats args; pyears = years args; ymin = fst pyears; ymax = snd pyears ; allcats = pcats==[]
        psubcats = subcats args ; allsubcats = psubcats==[]
        args = read ("Args {"++argstr++"}") :: Args ; nogroups = null grouptag || grouping == NoGrouping
        grouping = groups args ; grouptag = groupTag cfg

        itemsall = map doitem $ filter (ispostvisible ymin ymax "" "") $ Pages.allPosts page
        itemsfor (year:month:day:_) =
            map doitem $ filter (ispostvisible year year month day) $ Pages.allPosts page
        itemsfor (year:month:_) =
            map doitem $ filter (ispostvisible year year month "") $ Pages.allPosts page
        itemsfor (year:_) =
            map doitem $ filter (ispostvisible (max year ymin) (min year ymax) "" "") $ Pages.allPosts page
        itemsfor [] = [] -- for -Wall
        formatdate pfn
            | (length pfn)==1 = head pfn
            | (length pfn)==2 = let pdaters = Pages.daters page in (Util.keyVal pdaters "_MonthYear" (snd $ head pdaters)) (pfn++[""])
            | otherwise = (Util.keyVal daters dateformat (snd $ head daters)) pfn where daters = Pages.daters page
        dateformat = blogdf args
        doitem post = let datecat = (if null dateformat then "" else formatdate (Posts.fn post)) in
            (if (lean cfg) then buildLi_Lean else buildLi_Full) cfg args post datecat
