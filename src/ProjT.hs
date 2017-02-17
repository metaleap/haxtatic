module ProjT where

import Base
import qualified Lst

import qualified Util

import qualified Data.Map.Strict


parseProjChunks chunkssplits =
    Data.Map.Strict.fromList$ chunkssplits ~> Util.keepNoNilFsts foreach where
        foreach (tname:tvalsplits) =
            ( tname ~> Util.trim ,
                tvalsplits ~> (Lst.join ':') ~> Util.trim )
        foreach _ =
            ( "" , "" )


tagHandler ttags key =
    Data.Map.Strict.lookup key ttags
