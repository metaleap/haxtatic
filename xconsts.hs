module XConsts where

import Pages
import Util

type Cfg = Util.KeyVals

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr _ = [ Util.keyVal cfg argstr "" "" ]
