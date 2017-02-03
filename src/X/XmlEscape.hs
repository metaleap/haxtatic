{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.XmlEscape where

import qualified Html
import qualified X


registerX _ _ =
    X.Early$ \ (_ , argstr) -> Just$ Html.escape argstr
