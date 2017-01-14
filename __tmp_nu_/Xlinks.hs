{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Xlinks where

import Base
import qualified Html
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
    renderer (_ , argstr) =
        Just$ wrapitems allitems
        where
        allitems = htmlout (args-:htmlAtts ++ cfghtmlatts) (args-:items)
        args = Util.tryParse defargs errargs (("Args{"++).(++"}")) argstr where
            defargs = Args { items = [], htmlAtts = [] }
            errargs = Args { items = ["#"=:""], htmlAtts = X.htmlErrAttsArgs (xreg , Util.excerpt 23 argstr) }


    in X.Early renderer
    where


    htmlout atts argitems =
        argitems>~(foreach atts) ~> concat
    foreach attribs (url,text) =
        Html.out cfg_htmltagname attribs [ Html.T "a" ["" =: text , "href" =: cfgwraphref url] [] ]

    wrapitems = (cfgitemspre++).(++cfgitemspost)
    cfgitemspre = htmlout cfghtmlatts $cfg-:itemsFirst
    cfgitemspost = htmlout cfghtmlatts $cfg-:itemsLast

    (cfg_htmltagname , cfg_parsestr) = xreg-:X.cfgSplitOnce
    cfghtmlatts =  cfg-:htmlAtts
    cfgwraphref = cfg-:wrapHref ~> \(w1,w2) -> (w1++).(++w2)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { htmlAtts = [], itemsFirst = [], itemsLast = [], wrapHref = ("","") }
        errcfg = Cfg { htmlAtts = X.htmlErrAttsCfg xreg ,
                        itemsFirst = ["#"=:""], itemsLast = [], wrapHref = ("","") }
