{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html
import qualified Tmpl
import qualified Util
import Util ( (|?) , (|!) , (=:) , (~:) )

import qualified Data.Maybe
import qualified Text.Read




data Tag =
    Cfg {
        atts :: Util.StringPairs
    }
    deriving (Read)




registerX (xname , tname) _ (cfg_htmltagname , cfg_parsestr) =
    renderer cfg
    where

    renderer cfg _ctxpage argstr =
        Html.out cfg_htmltagname
                    ( cfg~:atts ++ [("" =: innercontent)] )
                        []
        where
            innercontent = argstr

    cfg = Util.tryParse defcfg errcfg $"Cfg"++cfg_parsestr where
        defcfg = Cfg { atts = [] }
        errcfg = Cfg { atts = [
                        "style" =: "background-color: red !important; color: yellow !important;",
                        "" =: "Parse error following `X|:"++xname++":"++tname++":"++cfg_htmltagname++":` in your *.haxproj"
                        ] }
