{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module XdemoSimplest where

import X



registerX _xreg =
    let
    renderer (_maybepagectx , _argstr) =

        Just$ "<h1>Hello World!</h1>"


    in Early renderer
