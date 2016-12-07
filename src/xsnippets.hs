module XSnippets where

import Pages
import Util

type Args = (String,Util.KeyVals)
type Cfg = Util.KeyVals

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr _ =
        [ Util.replace (Util.keyVal cfg (fst args) "") vars ] where
            args = (read ("("++argstr++")") :: Args)
            vars = map (\(k,v) -> ("{{"++k++"}}",v)) $ snd args
