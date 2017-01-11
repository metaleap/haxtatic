{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Xsnippet where

import Base
import qualified Util
import qualified X


data Tag
    = Cfg {
        vars :: Util.StringPairs,
        content :: String
    }
    | Args {
        vars :: Util.StringPairs,
        content :: String
    } deriving (Read)


registerX xreg =
    let
    renderer (_ , argstr) =
        Just "content"

    in X.Early renderer
    where
