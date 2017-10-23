module X.HtmlImage where

import Base
import qualified Str

import qualified Html
import qualified Util
import qualified X


data Tag =
    Cfg {
        attrLink :: Str.Pairs,
        attrImg :: Str.Pairs,
        xmlEscape :: Bool
    }
    deriving Read


registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ if null imgsrc then "" else Html.emit tag
        where
        haslink = has $cfg-:attrLink
        tag = if haslink then lnktag else imgtag
        imgtag = Html.T "img" (cfgimgatts ++ (atts "src" "alt")) []
        lnktag = Html.T "a" (cfglinkatts ++ (atts "href" "title")) [imgtag]
        (imgsrc,imgdesc) = argsplit ~> Str.trimBoth
        argsplit = Util.splitOn1stSpace argstr
        atts uriattname descattname =
            let imgtext = cfgerrmsg <?> (htmlesc imgdesc)
            in [ uriattname =: Html.joinUri cfg_imgrelpath imgsrc, descattname =: imgtext ]
            ++ if haslink || descattname == "title" then [] else
                [ "title" =: imgtext ]


    in X.Early renderer
    where


    htmlesc = if cfg-:xmlEscape then Html.escape else id
    (cfg_imgrelpath , cfg_parsestr) = xreg-:X.cfgSplitOnce
    (cfgerrmsg , cfgimgatts) = Html.attrClearInner $cfg-:attrImg
    (_ , cfglinkatts) = Html.attrClearInner $cfg-:attrLink
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { attrLink = [], attrImg = [], xmlEscape = False }
        errcfg = Cfg { attrLink = [], attrImg = X.htmlErrAttsCfg xreg, xmlEscape = False }
