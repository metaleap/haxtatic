module XSnippets where

import Pages
import Util

type Args = (String,Util.KeyVals)
type Cfg = Util.KeyVals

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr _ =
        [ Util.replace (Util.keyVal cfg (fst args) "" "") (snd args) ]
        where
            args = read ("("++argstr++")") :: Args
