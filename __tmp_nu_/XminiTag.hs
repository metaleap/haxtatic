{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html



registerX _name cfgstr =
    renderer
    where

    tagname = cfgstr

    renderer _ctxpage argstr =
        Html.out tagname [("",innercontent)] []
        where innercontent = argstr
