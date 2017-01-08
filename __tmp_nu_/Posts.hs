{-# OPTIONS_GHC -Wall #-}
module Posts where

import Base
import qualified Util

import qualified Data.Map.Strict


data Post
    = P {
        dt :: String,
        cat :: String,
        title :: String,
        link :: String,
        pic :: String,
        innerHtml :: String
    } deriving (Eq, Read)



parseProjChunks chunkssplits =
    (chunkssplits >~ foreach) ~> Util.unMaybes where
        foreach (pfeedcat:pvalsplits) =
            let
                -- pstr = Util.join ":" pvalsplits ~> Util.trim
                -- tmp = ( pfeedcat ~> Util.trim , pstr )
                post = P {

                    }
            in Just post
        foreach _ =
            Nothing


-- tagHandler ttags key =
--     Data.Map.Strict.lookup key ttags

writeAtoms =
    putStrLn "//"
