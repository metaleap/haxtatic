{-# OPTIONS_GHC -Wall #-}
module XdemoCfgArgs where



registerX (_ , tname) (cfgstr , _) (_ , _) =
    renderer
    where

    renderer _ctxpage argstr =
        "<i style='background: gold'>Hello, "++greet++"!</i>"
        where
        greet = if (not.null) argstr then argstr
                    else if (not.null) cfgstr then cfgstr
                        else tname
