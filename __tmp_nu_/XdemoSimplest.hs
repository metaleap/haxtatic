{-# OPTIONS_GHC -Wall #-}
module XdemoSimplest where


import qualified X
import qualified Tmpl

registerX :: X.Reg -> (Tmpl.CtxPage , String) -> String
registerX _xreg =
    renderer
    where

    renderer :: (Tmpl.CtxPage , String) -> String
    renderer (pagectx , _argstr) =
        "<h1>Hello "++(Tmpl.blokName pagectx)++"!</h1>"
