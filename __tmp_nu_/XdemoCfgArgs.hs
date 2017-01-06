{-# OPTIONS_GHC -Wall #-}
module XdemoCfgArgs where

import qualified X



registerX xreg =
    renderer
    where

    renderer argstr =
        "<h1>Hello, " ++ greet ++ "!</h1>"
        where
        greet = pick1of argstr
                        cfgstr
                        myname

        pick1of trydis trydat giveup =
            if (not.null) trydis then trydis
                else if (not.null) trydat then trydat
                    else giveup


    myname = X.tname xreg
    cfgstr = X.cfgFullStr xreg
