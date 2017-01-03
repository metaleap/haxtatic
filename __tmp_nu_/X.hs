{-# OPTIONS_GHC -Wall #-}
module X where

import qualified Util
import Util ( (~:) , (>~) , (~.) , (~|) , noNull )
import qualified XminiTag

import qualified Data.Map.Strict



parseProjLines linessplits =
    Data.Map.Strict.fromList$ linessplits>~foreach ~|fst~.noNull where
        foreach ("|X|":"miniTag":tname:tvals) =
            with XminiTag.registerX tname tvals
        foreach ("|X|":xname:tname:_) =
            ( tname~:Util.trim , rendererr xname )
        foreach _ =
            ( "" , id )
        with registerx tname tvals =
            ( tname~:Util.trim , registerx tname (Util.join ":" tvals) )
        rendererr xname _ctxpage _argstr =
            "{!X| Specified X-renderer `"++xname++"` not known |!}"



tagResolver xtags tagcontent =
    let
        (key , argstr) = Util.splitAt1st ':' tagcontent
        maybetag = Data.Map.Strict.lookup key xtags
    in case maybetag of
        Nothing -> Nothing
        Just xtag -> Just (xtag undefined argstr)
