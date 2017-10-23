module X.XmlEscape where

import qualified Html
import qualified X


registerX _ _ =
    X.Early$ \ (_ , argstr) -> Just$ Html.escape argstr
