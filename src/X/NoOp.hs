module X.NoOp where

import Base

import qualified X


registerX _ xreg =
    let constval = xreg-:X.cfgFullStr
    in X.Early$ if null constval
        then \ (_,argstr) -> Just argstr
        else \_ -> Just constval
