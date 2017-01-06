{-# OPTIONS_GHC -Wall #-}
module XdemoSimplest where

import X



registerX _xreg =
    let
    renderer (_maybepagectx , _argstr) =

        Just$ "<h1>Hello World!</h1>"


    in Early renderer
