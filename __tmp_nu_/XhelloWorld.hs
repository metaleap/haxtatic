{-# OPTIONS_GHC -Wall #-}
module XhelloWorld where



registerX (_xname , _tname) (cfgstr , _cfgvals) (_ , _) =
    renderer
    where

    renderer _ctxpage argstr =
        "<i style='background: gold'>Hello, "++greet++"!</i>"
        where
        greet = if (not.null) argstr then argstr
                    else if (not.null) cfgstr then cfgstr
                        else "world"
