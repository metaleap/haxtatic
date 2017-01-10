{-# OPTIONS_GHC -Wall #-}
module Ximage where

import Base
import qualified Html
import qualified Pages
import qualified Util
import qualified X

import System.FilePath ( (</>) )


data Tag =
    Cfg {
        lnkAtts :: Util.StringPairs,
        imgAtts :: Util.StringPairs
    }
    deriving (Read, Show)



registerX xreg =
    let
    renderer (_ , argstr) =

        Just$ Html.emit tag
        where
        tag = if null (cfg.:lnkAtts) then imgtag else lnktag
        imgtag = Html.T "img" (cfgimgatts ++ (atts "src" "alt")) []
        lnktag = Html.T "a" (cfglnkatts ++ (atts "href" "title")) [imgtag]
        (imgsrc,imgdesc) = (Util.splitOn1stSpace argstr) ~> Util.bothTrim
        atts uriattname descattname =
            [   uriattname =: Html.joinUri cfg_imgrelpath imgsrc,
                descattname =: Util.ifNo cfgerrmsg imgdesc ]


    in X.Early renderer
    where


    (cfg_imgrelpath , cfg_parsestr) = xreg.:X.cfgSplitOnce
    (cfgerrmsg , cfgimgatts) = Html.attrClearInner $cfg.:imgAtts
    (_ , cfglnkatts) = Html.attrClearInner $cfg.:lnkAtts
    cfg = Util.tryParse defcfg errcfg ("Cfg"++) cfg_parsestr where
        defcfg = Cfg { lnkAtts = [], imgAtts = [] }
        errcfg = Cfg { lnkAtts = [], imgAtts = X.htmlErrAttsCfg xreg }
