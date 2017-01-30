{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.NoOp where

import qualified X


registerX _ _ =
    X.Early (Just . snd)
