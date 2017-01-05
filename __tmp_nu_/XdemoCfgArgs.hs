{-# OPTIONS_GHC -Wall #-}
module XdemoCfgArgs where

import qualified X



registerX xreg =
    renderer
    where

    renderer _ctxpage argstr =
        "<i style='background: gold'>Hello, "++greet++"!</i>"
        where
        tname = X.tname xreg ; cfgstr = X.cfgFullStr xreg
        greet = if (not.null) argstr then argstr
                    else if (not.null) cfgstr then cfgstr
                        else tname
