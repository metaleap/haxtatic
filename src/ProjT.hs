module ProjT where

import Base
import qualified Lst
import qualified Str

import qualified Util

import qualified Data.Map.Strict


parseProjChunks chunkssplits =
    Data.Map.Strict.fromList$ chunkssplits ~> Util.keepNoNilFsts foreach where
        foreach (tname:tvalsplits) =
            ( tname ~> Str.trim ,
                tvalsplits ~> (Lst.join ':') ~> Str.trim )
        foreach _ =
            ( "" , "" )


tagHandler ttags key =
    Data.Map.Strict.lookup key ttags
