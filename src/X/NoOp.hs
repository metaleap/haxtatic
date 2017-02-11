{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.NoOp where

import Hax.Base

import qualified X


registerX _ xreg =
    let constval = xreg-:X.cfgFullStr
    in X.Early$ if null constval
        then \ (_,argstr) -> Just argstr
        else \_ -> Just constval
