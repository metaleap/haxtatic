{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.MiniTag where

import Base
import qualified Html
import qualified Util
import qualified X

import qualified Data.List



data Tag
    = Cfg {
        attr :: Util.StringPairs
    }
    deriving Read



registerX _ xreg =
    let
    renderplain (_ , argstr) =
        Just$ if null argstr then "" else cfg_htmltagplain argstr
    renderwithattr (_ , argstr) =
        Just$ (null argstr) |? "" |! Html.out
                cfg_htmltagname (cfghtmlatts ++ [("" =: argstr)] ++ (dynatts argstr)) []
        where
        dynatts inn = cfgdynatts >~ \(name , val) -> (name , repl val) where
            repl [] = []
            repl ('{':':':'c':':':'}':rest) = inn ++ repl rest
            repl (this:rest) = this : repl rest

    in X.Early (has (cfg-:attr) |? renderwithattr |! renderplain)
    where

    cfg_htmltagplain = ((pref).(suff)) where
                        suff = (++suffix) ; pref = (prefix++) . ((:) '>') where
                        prefix = '<':cfg_htmltagname ; suffix = ('<':'/':cfg_htmltagname)++">"
    (cfg_htmltagname , cfg_parsestr ) = xreg-:X.cfgSplitOnce
    (cfgdynatts , cfghtmlatts) = (cfg-:attr) ~> (Data.List.partition $(Data.List.isInfixOf "{:c:}").snd)
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { attr = [] }
        errcfg = Cfg { attr = X.htmlErrAttsCfg xreg }
