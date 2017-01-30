{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.NoOp where

import Base
import qualified X


registerX _ xreg =
    let constval = xreg-:X.cfgFullStr
    in if null constval
        then X.Early (Just . snd)
        else X.Early (\_ -> Just constval)
