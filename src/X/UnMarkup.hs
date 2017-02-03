{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.UnMarkup where

import qualified Html
import qualified X


registerX _ _ =
    X.Early$ \ (_ , argstr) -> Just$ Html.stripMarkup False ' ' argstr
