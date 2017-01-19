{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Snippet where

import Base
import qualified Tmpl
import qualified Util
import qualified X


data Tag
    = Cfg {
        vars :: Util.StringPairs,
        flags :: [String],
        content :: String
    }
    | Args {
        vars :: Util.StringPairs,
        flags :: [String],
        content :: String
    }
    deriving Read


registerX _ xreg =
    let
    renderer (_ , argstr) =
        Just$ Util.while again (Util.replaceSubs allrepls) (cfg-:content)
        where
        again ('{':'%':_) = True ; again ('%':'}':_) = True ; again (_:more) = again more ; again [] = False
        allrepls = argvars++cfgvars++flagrepls
        argvars = ("{%:content:%}" , args-:content) : (args-:vars >~ var2repl)
        flagrepls = concat$ (cfg-:flags) >~ flag2repl where
            flag2repl flag =
                let isflagset = elem flag (args-:flags)
                in [    "{%if:"++flag++"%}" =: (isflagset |? "" |! "<!--"),
                        "{%fi:"++flag++"%}" =: (isflagset |? "" |! "-->")]
        args = X.tryParseArgs (Tmpl.fixParseStr "content" argstr) (Just defargs) errargs where
            defargs = Args { vars = [], flags = [] , content = "" }
            errargs = Args { vars = [] , flags = [] , content = X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr)) }

    in X.Early renderer
    where

    var2repl (k,v) =
        "{%"++k++"%}" =: v

    cfgvars = (cfg-:vars) >~ var2repl
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { vars = [] , flags = [] , content = "" }
        errcfg = Cfg { vars = [] , flags = [] , content = X.htmlErr (X.clarifyParseCfgError xreg) }
