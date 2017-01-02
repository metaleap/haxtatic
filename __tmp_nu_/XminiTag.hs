{-# OPTIONS_GHC -Wall #-}
module XminiTag where



register name cfgstr =
    renderer
    where

    renderer _ argstr =
        enclose argstr

    enclose = (('<':cfgstr++">") ++) . (++ ("</"++cfgstr++">"))
