{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.DemoSimplest where

import X



registerX _project _tag_config =
    let
    renderer (_page , _tag_args) =

        Just "<h1>Hello World!</h1>"


    in Early renderer
