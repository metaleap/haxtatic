{-# OPTIONS_GHC -Wall #-}
module XminiTag where

import qualified Html
import qualified Tmpl
import qualified Util
import Util ( (|?) , (|!) , (=:) , (~:) )

import qualified Data.Maybe
import qualified Text.Read


data Cfg =
	Cfg {
		atts :: Util.StringPairs
	}
	deriving (Read)



registerX (xname , tname) (_cfgstr , _cfgvals) (tagname , cfgstr) =
    renderer cfg
    where
    cfg = (null cfgstr) |? cfgdef |! Data.Maybe.fromMaybe cfgerr cfgmaybe where
    		cfgdef = Cfg { atts = [] }
    		cfgerr = Cfg { atts = [
                            "style" =: "background-color: red !important; color: yellow !important;",
                            "" =: "Parse error following `X|:"++xname++":"++tname++":"++tagname++":` in your *.haxproj"
                            ] }
    		cfgmaybe = (Text.Read.readMaybe $"Cfg"++cfgstr) :: Maybe Cfg

    renderer cfg _ctxpage argstr =
        Html.out tagname
                    ( cfg~:atts ++ [("" =: innercontent)] )
                        []
        where innercontent = argstr
