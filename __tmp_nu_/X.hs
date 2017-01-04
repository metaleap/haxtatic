{-# OPTIONS_GHC -Wall #-}
module X where

import qualified Util
import Util ( (~:) , (>~) , (~.) , (~|) , (=:) , (|?) , (|!) , noNull )

import qualified XhelloWorld
import qualified XminiTag

import qualified Data.List
import qualified Data.Map.Strict



parseProjLines linessplits =
    Data.Map.Strict.fromList$ linessplits>~foreach ~|fst~.noNull where
        xnames = [  ("haxHelloWorld" =: XhelloWorld.registerX),
                    ("haxMiniTag" =: XminiTag.registerX)
                    ]
        foreach ("|X|":xname:tname:tvals) =
            let xn = Util.trim xname
                tn = Util.trim tname
            in if null tn then ( "" , id ) else
                case Data.List.lookup xn xnames of
                    Nothing -> ( tn , rendererr ("{!X| Specified X-renderer `"++xname++"` not known |!}") )
                    Just registerx -> with registerx xn tn tvals
        foreach _ =
            ( "" , id )
        with registerx xname tname tvals =
            ( tname ,
                registerx (xname , tname)
                            (cfgstr , cfgvals)
                                cfgsplit
                ) where
                    cfgstr = Util.trim$ Util.join ":" tvals
                    cfgvals = tvals>~Util.trim
                    cfgsplit = Util.both' Util.trim (Util.splitAt1st ':' cfgstr)
        rendererr msg _ctxpage _argstr =
            msg



tagResolver xtags tagcontent =
    let
        (key , argstr) = Util.splitAt1st ':' tagcontent
        maybetag = Data.Map.Strict.lookup key xtags
    in case maybetag of
        Nothing -> Nothing
        Just xtag -> Just (xtag undefined (Util.trim argstr))
