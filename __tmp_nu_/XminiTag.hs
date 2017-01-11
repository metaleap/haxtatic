{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module XminiTag where

import Base
import qualified Html
import qualified Util
import qualified X


data Tag
    = Cfg {
        htmlAtts :: Util.StringPairs
    }
    deriving (Read)



registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Html.out
                cfg_htmltagname ( cfghtmlatts ++ [("" =: innercontent)] ) []
        where
        innercontent = argstr

    in X.Early renderer
    where

    (cfg_htmltagname , cfg_parsestr ) = xreg-:X.cfgSplitOnce
    cfghtmlatts =  cfg-:htmlAtts
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { htmlAtts = [] }
        errcfg = Cfg { htmlAtts = X.htmlErrAttsCfg xreg }
