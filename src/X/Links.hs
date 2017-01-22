{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Links where

import Base
import qualified Files
import qualified Html
import qualified Tmpl
import qualified Util
import qualified X


data Tag =
    Cfg {
        htmlAtts :: Util.StringPairs,
        itemsFirst :: Util.StringPairs,
        itemsLast :: Util.StringPairs,
        wrapHref :: (String , String)
    }
    | Args {
        items :: Util.StringPairs,
        htmlAtts :: Util.StringPairs
    }
    deriving Read



registerX _ xreg =
    let
    renderer (maybectxpage , argstr) =
        if waitforpage then Nothing
            else Just$ combine allitems
        where
        allitems = htmlout (args-:htmlAtts ++ cfghtmlatts) (args-:items)
        args = X.tryParseArgs xreg argstr
                {-empty-} (Just Args { items = [], htmlAtts = [] })
                {-error-} (Args { items = ["#"=:""], htmlAtts = X.htmlErrAttsArgs (xreg , Util.excerpt 23 argstr) })

        htmlout atts argitems =
            argitems>~(foreach atts) ~> concat
        foreach attribs (url,text) =
            Html.out cfg_htmltagname
                        (attribs >=~ (outattr maybectxpage))
                            [ Html.T "a" ["" =: text , "href" =: cfgwraphref url] [] ]
            where
            outattr (Just ctxpage) (('&':name) , value) =
                if pathmatch then Just (name , value) else Nothing
                where
                pathmatch = (curdir == dstdir)
                curdir = Files.sanitizeUriRelPathForJoin url
                dstdir = ((ctxpage-:Tmpl.pTagHandler) "dirUri") ~> (Files.sanitizeUriRelPathForJoin =|- "")
            outattr _ other =
                Just other

        combine = (cfgitemspre++).(++cfgitemspost)
        cfgitemspre = htmlout cfghtmlatts $cfg-:itemsFirst
        cfgitemspost = htmlout cfghtmlatts $cfg-:itemsLast

        waitforpage =
            (X.hasNoPageContext maybectxpage) && (needpage4cfg || needpage4args)
            where
            needpage4args = (X.htmlAttsNeedPage $args-:htmlAtts) || (needpage $args-:htmlAtts)

    in X.EarlyOrWait renderer
    where


    needpage =
        any ispathconditional where
        ispathconditional (('&':_),_) = True
        ispathconditional _ = False
    needpage4cfg = X.htmlAttsNeedPage cfghtmlatts || needpage cfghtmlatts
    (cfg_htmltagname , cfg_parsestr) = xreg-:X.cfgSplitOnce
    cfghtmlatts =  cfg-:htmlAtts
    cfgwraphref = cfg-:wrapHref ~> \(w1,w2) -> (w1++).(++w2)
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { htmlAtts = [], itemsFirst = [], itemsLast = [], wrapHref = ("","") }
        errcfg = Cfg { htmlAtts = X.htmlErrAttsCfg xreg,
                        itemsFirst = ["#"=:""], itemsLast = [], wrapHref = ("","") }
