{-# OPTIONS_GHC -Wall #-}
module XpageAnchors where

import Base
import qualified Html
import qualified Util
import qualified X


data Tag
    = Cfg {
        considerEmpty :: Int,
        htmlIfEmpty :: String
    }
    | Args {
        htmlAtts :: Util.StringPairs
    } deriving (Read)



registerX xreg =
    let
    renderer (_ , argstr) =

        Just$ "<div>bla</div>"
        where


    in X.WaitForPage renderer
    where

    (cfg_gathertagname , cfg_parsestr ) = xreg.:X.cfgSplitOnce
    cfg = Util.tryParseOr errcfg ("Cfg"++ cfg_parsestr) where
        errcfg = let a = X.htmlErrAttsCfg xreg in Cfg {
                    htmlIfEmpty = (a#0)~>snd , considerEmpty = maxBound::Int }
