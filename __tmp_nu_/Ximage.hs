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



registerX xntn (_ , _) (cfg_imgrelpath , cfg_parsestr) =
    renderer cfg
    where

    renderer cfg _ctxpage argstr =
        Html.emit False tag
        where
        tag = if null (cfg~:lnkAtts) then imgtag else lnktag

        (imgsrc,imgdesc) = (Util.splitAt1st ':' argstr) ~: (Util.both' Util.trim)
        imgtag = Html.T "img" (cfgimgatts ++ imgatts) []
        imgatts = myatts "src" "title"
        lnktag = Html.T "a" (cfglnkatts ++ lnkatts) [imgtag]
        lnkatts = myatts "href" "title"
        myatts uriattname descattname =
            Html.attrEscapeVals [uriattname =: Html.joinUri cfg_imgrelpath imgsrc,
                                    descattname =: Util.ifNo cfgerrmsg imgdesc ]

    (cfgerrmsg , cfgimgatts) = Html.attrClearInner (Html.attrEscapeVals $cfg~:imgAtts)
    (_ , cfglnkatts) = Html.attrClearInner (Html.attrEscapeVals $cfg~:lnkAtts)
    cfg = Util.tryParse defcfg errcfg $"Cfg"++cfg_parsestr where
        defcfg = Cfg { lnkAtts = [], imgAtts = [] }
        errcfg = Cfg { lnkAtts = [], imgAtts = X.htmlAttsForParseError xntn cfg_imgrelpath }
