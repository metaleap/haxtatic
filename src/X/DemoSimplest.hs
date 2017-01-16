{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.DemoSimplest where

import X



registerX _ctxproj _xreg =
    let
    renderer (_maybepagectx , _argstr) =

        Just$ "<h1>Hello World!</h1>"


    in Early renderer
