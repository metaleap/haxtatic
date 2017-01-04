{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html
import qualified Tmpl
import qualified Util
import Util ( (|?) , (|!) , (=:) , (~:) )
import qualified X

import qualified Data.Maybe
import qualified Text.Read




data Tag =
    Cfg {
        htmlAtts :: Util.StringPairs
    }
    deriving (Read)




registerX xntn _ (cfg_htmltagname , cfg_parsestr) =
    renderer cfg
    where

    renderer cfg _ctxpage argstr =
        Html.out cfg_htmltagname
                    (False , cfghtmlatts ++ [("" =: innercontent)])
                        []
        where
        innercontent = argstr

    cfghtmlatts =  Html.attrEscapeVals $cfg~:htmlAtts
    cfg = Util.tryParse defcfg errcfg $"Cfg"++cfg_parsestr where
        defcfg = Cfg { htmlAtts = [] }
        errcfg = Cfg { htmlAtts = X.htmlAttsForParseError xntn cfg_htmltagname }
