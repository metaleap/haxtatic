{-# OPTIONS_GHC -Wall #-}
module Config where


import qualified Blogs
import qualified Pages
import qualified Util

import XImage
import XLinks
import XListing
import XMarkup
import XPageToc
import XRandom
import XRepeater
import XSnippets


_split :: String->[String]
_split = Util.splitBy ':'

_join :: [String]->String
_join = Util.join ":"

_trim0 :: String->String
_trim0 = Util.dropWhile '0'


blogs::
    [String]->
    [Blogs.Blog]
blogs cfglines =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("B":name:rest) = [(read ("Blog { name=\""++name++"\", "++(_join rest)++" }") :: Blogs.Blog)]
        persplit _ = []


exts::
    [String]-> [String]->
    [Pages.X]
exts cfglines blognames =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("X":"Image":name:rest) = [XImage.ext name (read (wc rest) :: XImage.Cfg)]
        persplit ("X":"Links":name:rest) = [XLinks.ext name (read (wc rest) :: XLinks.Cfg) blognames]
        persplit ("X":"Listing":name:rest) = [XListing.ext name (read (wc rest) :: XListing.Cfg)]
        persplit ("X":"Markup":name:rest) = [XMarkup.ext name $ Util.trimStart $ _join rest]
        persplit ("X":"PageToc":name:rest) = [XPageToc.ext name (read (wc rest) :: XPageToc.Cfg)]
        persplit ("X":"Random":name:rest) = [XRandom.ext name (read (wc rest) :: XRandom.Cfg)]
        persplit ("X":"Repeater":name:rest) = [XRepeater.ext name (read (wc rest) :: XRepeater.Cfg)]
        persplit ("X":"Snippets":name:rest) = [XSnippets.ext name (read (wl rest) :: XSnippets.Cfg)]
        persplit _ = []
        wl = wrap "[" "]" ; wc = wrap "Cfg {" "}"
        wrap pre post splits = pre++(_join splits)++post


txts::
    [String]->
    Util.KeyVals
txts cfglines =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("T":name:rest) = [("{T{"++name++"}}", Util.trimStart $ _join rest)]
        persplit _ = []


_daters::
    [String]->
    Util.KeyVals
_daters cfglines =
    [("","YYYY/MM/DD"),("_MonthYear","month YYYY"),("_Year","YYYY")] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("D":name:rest) = [(name, Util.trimStart $ _join rest)]
        persplit _ = []

daters::
    [String]-> (String->String)->
    [(String, ([String]->String))]
daters cfglines monthname =
    map makedater (_daters cfglines) where
        makedater (name,dateformat) = (name,dater) where
            dater = \(year:month:day:_) -> Util.replaceIn dateformat [
                ("mth", Util.take3 $ monthname month),
                ("month", monthname month),
                ("M", _trim0 month),
                ("MM",month),
                ("/MM",if null month then "" else "/"++month),
                ("D", _trim0 day),
                ("DD",day),
                ("/DD",if null day then "" else "/"++day),
                ("YY", drop 2 year),
                ("YYYY",year)
                ]
