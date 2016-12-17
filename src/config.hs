module Config where

import qualified Blogs
import qualified Util

--import XConsts
import XImage
import XLinks
import XListing
import XPageToc
import XRandom
import XRepeater
import XSnippets


_split = Util.splitBy ':'
_join = Util.join ":"
_trim0 = Util.trimChar '0'


blogs cfglines =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("B":name:rest) = [(read ("Blog { name=\""++name++"\", "++(_join rest)++" }") :: Blogs.Blog)]
        persplit _ = []


exts cfglines blognames =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        --persplit ("X":"Consts":name:rest) = [XConsts.ext name (read (wl rest) :: XConsts.Cfg)]
        persplit ("X":"Snippets":name:rest) = [XSnippets.ext name (read (wl rest) :: XSnippets.Cfg)]
        persplit ("X":"Links":name:rest) = [XLinks.ext name (read (wc rest) :: XLinks.Cfg) blognames]
        persplit ("X":"Image":name:rest) = [XImage.ext name (read (wc rest) :: XImage.Cfg)]
        persplit ("X":"PageToc":name:rest) = [XPageToc.ext name (read (wc rest) :: XPageToc.Cfg)]
        persplit ("X":"Listing":name:rest) = [XListing.ext name (read (wc rest) :: XListing.Cfg)]
        persplit ("X":"Random":name:rest) = [XRandom.ext name (read (wc rest) :: XRandom.Cfg)]
        persplit ("X":"Repeater":name:rest) = [XRepeater.ext name (read (wc rest) :: XRepeater.Cfg)]
        persplit _ = []
        wl = wrap "[" "]" ; wc = wrap "Cfg {" "}"
        wrap pre post splits = pre++(_join splits)++post


txts cfglines =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("T":name:rest) = [("{T{"++name++"}}", Util.trimSpace $ _join rest)]
        persplit _ = []


_daters cfglines =
    [("","YYYY/MM/DD"),("_MonthYear","month YYYY"),("_Year","YYYY")] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit $ _split ln
        persplit ("D":name:rest) = [(name, Util.trimSpace $ _join rest)]
        persplit _ = []

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
