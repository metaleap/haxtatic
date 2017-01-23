module X.DemoCfgArgs where

import Base (has)
import qualified Html
import qualified X


registerX _ctxproj xreg =
    let
    renderer (_maybectxpage , argstr) =
        Just$ "<h1>Hello, " ++ (greet argstr) ++ "!</h1>"

    in X.Early renderer
    where

    greet name
        | has name = name
        | has cfgstr = cfgstr
        | otherwise = fallback

    cfgstr = X.cfgFullStr xreg
    fallback = X.tname xreg
