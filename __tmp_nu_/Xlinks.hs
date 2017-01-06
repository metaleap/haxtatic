{-# OPTIONS_GHC -Wall #-}
module Xlinks where

import Base
import qualified Html
import qualified Tmpl
import qualified Util
import qualified X


data Tag =
    Cfg {
        htmlAtts :: Util.StringPairs,
        itemsFirst :: Util.StringPairs,
        itemsLast :: Util.StringPairs
    }
    | Args {
        items :: Util.StringPairs,
        htmlAtts :: Util.StringPairs
    }
    deriving (Read)




registerX xreg =
    let
    renderer (_ , argstr) =

        Just$ cfgitemspre ++ allitems ++ cfgitemspost
        where
        allitems = htmlout (args.:htmlAtts ++ cfghtmlatts) (args.:items)
        args = Util.tryParse defargs errargs (("Args{"++).(++"}")) argstr where
            defargs = Args { items = [], htmlAtts = [] }
            errargs = Args { items = ["#"=:""], htmlAtts = X.htmlAttsForArgsParseError xreg (Util.excerpt 23 argstr) }


    in X.Early renderer
    where




    htmlout atts items = concat$ items>~(foreach atts)
    foreach attribs (url,text) =
        Html.out cfg_htmltagname attribs [ Html.T "a" ["" =: text , "href" =: url] [] ]

    cfgitemspre = htmlout cfghtmlatts $cfg.:itemsFirst
    cfgitemspost = htmlout cfghtmlatts $cfg.:itemsLast

    (cfg_htmltagname , cfg_parsestr ) = xreg.:X.cfgSplitOnce
    cfghtmlatts =  cfg.:htmlAtts
    cfg = Util.tryParse defcfg errcfg ("Cfg"++) cfg_parsestr where
        defcfg = Cfg { htmlAtts = [],
                        itemsFirst = [], itemsLast = [] }
        errcfg = Cfg { htmlAtts = X.htmlAttsForCfgParseError xreg ,
                        itemsFirst = ["#"=:""], itemsLast = [] }
