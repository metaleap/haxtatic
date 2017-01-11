{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Xrepeat where

import Base
import qualified Tmpl
import qualified Util
import qualified X


data Tag
    = Cfg {
        prefix :: String,
        suffix :: String,
        joinwith :: String,
        content :: String
    }
    | Args {
        range :: Maybe (Int , Int),
        values :: [String]
    } deriving (Read)


registerX xreg =
    let
    renderer (_ , argstr) =
        Just$ (cfg-:prefix) ++ allcontents ++ (cfg-:suffix)
        where
        allcontents = Util.join (cfg-:joinwith) (iteratees >~ foreach)
        foreach (i,v) =
            Util.replaceSubs ["@:i:" =: show i , "@:v:" =: v] (cfg-:content)
        iteratees = Util.indexed$ case args-:range of
                        Nothing -> args-:values
                        Just (from,to) -> [from..to] >~ show
        args = X.tryParseArgs argstr (Just defargs) errargs where
            defargs = Args { range = Nothing , values = [] }
            errargs = Args { range = Nothing , values = [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))] }

    in X.Early renderer
    where

    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinwith = "" , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinwith = "" , content = "" }
