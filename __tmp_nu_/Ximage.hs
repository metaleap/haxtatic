{-# OPTIONS_GHC -Wall #-}
module Ximage where

import qualified Html
import qualified Pages
import qualified Util
import Util ( (=:) , (~:) )
import qualified X

import System.FilePath ( (</>) )


data Tag =
    Cfg {
        lnkAtts :: Util.StringPairs,
        imgAtts :: Util.StringPairs
    }
    deriving (Read, Show)



registerX xreg =
    renderer cfg
    where

    renderer cfg _ctxpage argstr =
        Html.emit tag
        where
        tag = if null (cfg~:lnkAtts) then imgtag else lnktag

        (imgsrc,imgdesc) = (Util.splitAt1stSpace argstr) ~: (Util.both' Util.trim)
        imgtag = Html.T "img" (cfgimgatts ++ imgatts) []
        imgatts = myatts "src" "title"
        lnktag = Html.T "a" (cfglnkatts ++ lnkatts) [imgtag]
        lnkatts = myatts "href" "title"
        myatts uriattname descattname =
            [uriattname =: Html.joinUri cfg_imgrelpath imgsrc,
                            descattname =: Util.ifNo cfgerrmsg imgdesc ]

    (cfg_imgrelpath , cfg_parsestr) = xreg~:X.cfgSplitOnce
    (cfgerrmsg , cfgimgatts) = Html.attrClearInner $cfg~:imgAtts
    (_ , cfglnkatts) = Html.attrClearInner $cfg~:lnkAtts
    cfg = Util.tryParse defcfg errcfg $"Cfg"++cfg_parsestr where
        defcfg = Cfg { lnkAtts = [], imgAtts = [] }
        errcfg = Cfg { lnkAtts = [], imgAtts = X.htmlAttsForParseError xreg }
