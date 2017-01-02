{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html



register _name cfgstr =
    renderer
    where

    renderer _ctxpage argstr =
        enclose argstr

    enclose = (('<':cfgstr++">") ++) . (++ ("</"++cfgstr++">"))
