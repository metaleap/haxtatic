{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.MiniTag where

import Base
import qualified Html
import qualified Util
import qualified X

import qualified Data.List



data Tag
    = Cfg {
        htmlAtts :: Util.StringPairs
    }
    deriving Read



registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Html.out
                cfg_htmltagname (cfghtmlatts ++ [("" =: innercontent)] ++ (dynatts innercontent)) []
        where
        innercontent = argstr
        dynatts inn = cfgdynatts >~ \(name , _) -> (name , Html.escape inn)

    in X.Early renderer
    where

    (cfg_htmltagname , cfg_parsestr ) = xreg-:X.cfgSplitOnce
    (cfgdynatts , cfghtmlatts) = (cfg-:htmlAtts) ~> (Data.List.partition $("{%:content:%}"==).snd)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { htmlAtts = [] }
        errcfg = Cfg { htmlAtts = X.htmlErrAttsCfg xreg }
