{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module XdemoCfgArgs where

import qualified X


registerX _ctxproj xreg =
    let
    renderer (_ , argstr) =

        Just$ "<h1>Hello, " ++ greet ++ "!</h1>"
        where
        greet = pick1of argstr
                        cfgstr
                        myname


    in X.Early renderer
    where


    myname = X.tname xreg
    cfgstr = X.cfgFullStr xreg


pick1of trydis trydat giveup
    |(not.null)trydis=  trydis
    |(not.null)trydat=  trydat
    |(otherwise)=       giveup
