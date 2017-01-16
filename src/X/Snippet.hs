{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Snippet where

import Base
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
        Just$ Util.replaceSubs (argvars++cfgvars) (cfg-:content)
        where
        argvars = ("[:content:]" , args-:content) : (args-:vars >~ torepl)
        args = X.tryParseArgs (Tmpl.fixParseStr "content" argstr) (Just defargs) errargs where
            defargs = Args { vars = [] , content = "" }
            errargs = Args { vars = [] , content = X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr)) }

    in X.Early renderer
    where

    torepl (k,v) =
        "[|"++k++"|]" =: v

    cfgvars = (cfg-:vars) >~ torepl
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { vars = [] , content = "" }
        errcfg = Cfg { vars = [] , content = X.htmlErr (X.clarifyParseCfgError xreg) }
