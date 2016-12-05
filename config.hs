module Config where

import Blogs
import Util

import XConsts
import XImage
import XLinks
import XListing
import XPageToc
import XRandom
import XSnippets


blogs cfglines =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit (Util.splitBy ':' ln)
        persplit ("Blog":name:rest) = [(read ("Blog { name=\""++name++"\", "++(Util.join ":" rest)++" }") :: Blogs.Blog)]
        persplit _ = []


exts cfglines =
    [] ++ (concat $ map perline cfglines) where
        perline "" = [] ; perline ln = persplit (Util.splitBy ':' ln)
        persplit ("XConsts":name:rest) = [XConsts.ext name (read (wl rest) :: XConsts.Cfg)]
        persplit ("XSnippets":name:rest) = [XSnippets.ext name (read (wl rest) :: XSnippets.Cfg)]
        persplit ("XLinks":name:rest) = [XLinks.ext name (read (wc rest) :: XLinks.Cfg)]
        persplit ("XImage":name:rest) = [XImage.ext name (read (wc rest) :: XImage.Cfg)]
        persplit ("XPageToc":name:rest) = [XPageToc.ext name (read (wc rest) :: XPageToc.Cfg)]
        persplit ("XListing":name:rest) = [XListing.ext name (read (wc rest) :: XListing.Cfg)]
        persplit ("XRandom":name:rest) = [XRandom.ext name (read (wc rest) :: XRandom.Cfg)]
        persplit _ = []
        wl = wrap "[" "]" ; wc = wrap "Cfg {" "}"
        wrap pre post splits = pre++(Util.join ":" splits)++post
