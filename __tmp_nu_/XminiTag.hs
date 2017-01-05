{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import Base
import qualified Html
import qualified Tmpl
import qualified Util
import qualified X



data Tag =
    Cfg {
        htmlAtts :: Util.StringPairs
    }
    deriving (Read)




registerX xreg =
    renderer cfg
    where

    renderer cfg _ctxpage argstr =
        Html.out cfg_htmltagname
                    (cfghtmlatts ++ [("" =: innercontent)])
                        []
        where
        innercontent = argstr



    (cfg_htmltagname , cfg_parsestr ) = xreg~:X.cfgSplitOnce
    cfghtmlatts =  cfg~:htmlAtts
    cfg = Util.tryParse defcfg errcfg ("Cfg"++) cfg_parsestr where
        defcfg = Cfg { htmlAtts = [] }
        errcfg = Cfg { htmlAtts = X.htmlAttsForCfgParseError xreg }
