module X.DemoCfgArgs where

import Base

import qualified Html
import qualified X


registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Html.out "h4" ["" =: "Hello, "]
                            [Html.T "b" ["" =: greet argstr ++ "!"] []]

    in X.Early renderer
    where

    greet name
        | has name = name
        | has cfgname = cfgname
        | otherwise = fallbackname

    cfgname = xreg-:X.cfgFullStr
    fallbackname = xreg-:X.tname
