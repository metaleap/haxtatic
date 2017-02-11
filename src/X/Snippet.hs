{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Snippet where

import Hax.Base

import qualified Tmpl
import qualified Util
import qualified X


data Tag
    = Cfg {
        vars :: Util.StringPairs,
        content :: String
    }
    | Args {
        vars :: Util.StringPairs,
        content :: String
    }
    deriving Read


registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Util.repeatedly (Util.replaceSubsMany allrepls) maincontent
        where
        maincontent = Util.lookup "_hax_snippeterror" (cfg-:content) ((args-:vars)++(cfg-:vars))
        allrepls = argvars++cfgvars
        argvars = ("{:c:}" , args-:content) : (args-:vars >~ var2repl)
        args = X.tryParseArgs xreg (Tmpl.fixParseStr "content" argstr) (Just defargs) errargs where
            defargs = Args { vars = [], content = "" }
            errargs = Args { vars = ["_hax_snippeterror" =: errmsg] , content = errmsg }
            errmsg = X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))

    in X.Early renderer
    where

    var2repl (k,v) =
        "{%"++k++"%}" =: v

    cfgvars = (cfg-:vars) >~ var2repl
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg xreg cfg_parsestr Nothing errcfg where
        errcfg = Cfg { vars = ["_hax_snippeterror" =: errmsg] , content = errmsg }
        errmsg = X.htmlErr (X.clarifyParseCfgError xreg)
