{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Link where

import Base
import qualified Files
import qualified Html
import qualified Util
import qualified X


data Tag =
    Cfg {
        attr :: Util.StringPairs,
        xmlEscape :: (Bool , Bool),
        uriAutoExt :: String
    }
    deriving Read


registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Html.emit tag
        where
        tag = Html.T "a" (cfglinkatts ++ (atts "href" "")) []
        (linkhref,linktext) = argsplit ~> Util.bothTrim ~> htmlescape
        argsplit = Util.splitOn1stSpace argstr
        atts uriattname descattname =
            [   uriattname =: Html.joinUri cfg_relpath (uriautoext linkhref),
                descattname =: Util.ifNo cfgerrmsg linktext ]


    in X.Early renderer
    where


    uriautoext =
        for (cfg-:uriAutoExt)
        where
        for "" href = href
        for ext href =
            let (hrefpath , hrefanchor) = Util.splitOn1st '#' href
            in (Files.ensureFileExt False ext hrefpath) ++ (null hrefanchor |? "" |! ('#':hrefanchor))
    htmlescape =
        htmlesc $cfg-:xmlEscape where
        htmlesc (False , False) = id
        htmlesc (True , True) = Util.both' Html.escape
        htmlesc (forhref , fortext) = Util.both ((forhref |? Html.escape |! id) , (fortext |? Html.escape |! id))
    (cfg_relpath , cfg_parsestr) = xreg-:X.cfgSplitOnce
    (cfgerrmsg , cfglinkatts) = Html.attrClearInner $cfg-:attr
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { attr = [] , xmlEscape = (False , False) , uriAutoExt = "" }
        errcfg = Cfg { attr = X.htmlErrAttsCfg xreg , xmlEscape = (False , False) , uriAutoExt = "" }
