module X.HtmlAnchors where

import Base

import qualified Html
import qualified Tmpl
import qualified Util
import qualified X


data Tag
    = Cfg {
        considerEmpty :: Int,
        outputIfEmpty :: String,
        xmlEscapeHref :: Bool
    }
    | Args {
        attr :: Util.StringPairs
    }
    deriving Read



registerX _ xreg =
    let
    renderer (Just pagectx , argstr)
        | ((cfg-:considerEmpty) == (maxBound::Int))
            || null tagmatches
            || (tagmatches~>length) <= (cfg-:considerEmpty)
        = Just$ cfg-:outputIfEmpty
        | otherwise
        = Just$ tagmatches >>= foreach
        where

        tagmatches = (pagectx-:Tmpl.htmlInners) cfg_gathertagname
        foreach tagmatch =
            Html.out args_tagname (args-:attr)
                        [Html.T "a" ["" =: tagmatch , "href" =: "#"++(xmlesc tagmatch)] []]
        (args_tagname , args_parsestr) = Util.splitOn1st_ ':' argstr
        args = X.tryParseArgs xreg args_parsestr
                (Just Args { attr = [] })
                (Args { attr = X.htmlErrAttsArgs (xreg , Util.excerpt 23 argstr) })

    renderer _ =
        Nothing

    in X.WaitForPage renderer
    where

    xmlesc = if cfg-:xmlEscapeHref then Html.escape else id
    (cfg_gathertagname , cfg_parsestr ) = xreg-:X.cfgSplitOnce
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { outputIfEmpty = "" , considerEmpty = 0 , xmlEscapeHref = False }
        errcfg = Cfg { outputIfEmpty = X.htmlErr$ X.clarifyParseCfgError xreg , considerEmpty = maxBound::Int , xmlEscapeHref = False }
