{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html



register _name cfgstr =
    renderer
    where

    renderer _ctxpage argstr =
    	let wot = Html.out cfgstr [("","argstr") , ("test","bla")] []
        in wot -- enclose (show argstr)

    enclose = (('<':cfgstr++">") ++) . (++ ("</"++cfgstr++">"))
