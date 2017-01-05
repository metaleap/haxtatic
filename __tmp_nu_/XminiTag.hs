{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html
import qualified Tmpl
import qualified Util
import Util ( (|?) , (|!) , (=:) , (~:) )
import qualified X

import qualified Data.Maybe
import qualified Text.Read




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
    cfg = Util.tryParse defcfg errcfg $"Cfg"++cfg_parsestr where
        defcfg = Cfg { htmlAtts = [] }
        errcfg = Cfg { htmlAtts = X.htmlAttsForParseError xreg }
        xntn = (xreg~:X.xname , xreg~:X.tname)
