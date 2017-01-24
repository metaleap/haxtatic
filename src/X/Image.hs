{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Image where

import Base
import qualified Html
import qualified Util
import qualified X


data Tag =
    Cfg {
        attrLink :: Util.StringPairs,
        attrImg :: Util.StringPairs
    }
    deriving Read


registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Html.emit tag
        where
        tag = if null (cfg-:attrLink) then imgtag else lnktag
        imgtag = Html.T "img" (cfgimgatts ++ (atts "src" "alt")) []
        lnktag = Html.T "a" (cfglinkatts ++ (atts "href" "title")) [imgtag]
        (imgsrc,imgdesc) = argsplit ~> Util.bothTrim
        argsplit = Util.splitOn1stSpace argstr
        atts uriattname descattname =
            [   uriattname =: Html.joinUri cfg_imgrelpath imgsrc,
                descattname =: Util.ifNo cfgerrmsg imgdesc ]


    in X.Early renderer
    where


    (cfg_imgrelpath , cfg_parsestr) = xreg-:X.cfgSplitOnce
    (cfgerrmsg , cfgimgatts) = Html.attrClearInner $cfg-:attrImg
    (_ , cfglinkatts) = Html.attrClearInner $cfg-:attrLink
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { attrLink = [], attrImg = [] }
        errcfg = Cfg { attrLink = [], attrImg = X.htmlErrAttsCfg xreg }
