{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module XpageAnchors where

import Base
import qualified Tmpl
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
    renderer (Just pagectx , argstr)
        | ((cfg-:considerEmpty) == (maxBound::Int))
        || null tagmatches
        || (tagmatches~>length) <= (cfg-:considerEmpty)
        = Just$ cfg-:htmlIfEmpty
        | otherwise
        = Just$ concat$ tagmatches>~foreach
        where

        foreach tagmatch =
            "<a href=\"#\">"++tagmatch++"</a>"
        tagmatches = (pagectx-:Tmpl.htmlInners) cfg_gathertagname

    in X.WaitForPage renderer
    where

    (cfg_gathertagname , cfg_parsestr ) = xreg-:X.cfgSplitOnce
    cfg = X.tryParseCfg cfg_parsestr Nothing errcfg where
        errcfg = Cfg { htmlIfEmpty = X.htmlErr$ X.clarifyParseCfgError xreg , considerEmpty = maxBound::Int }
