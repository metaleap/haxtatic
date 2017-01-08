{-# OPTIONS_GHC -Wall #-}
module ProjT where

import Base
import qualified Util

import qualified Data.Map.Strict


parseProjChunks canparsestr chunkssplits =
    Data.Map.Strict.fromList$ chunkssplits>~foreach ~|is.fst where
        foreach (tname:tvalsplits) =
            ( tname ~> Util.trim ,
                tvalsplits ~> (Util.join ":") ~> Util.trim )
        foreach _ =
            ( "" , "" )


tagHandler ttags key =
    Data.Map.Strict.lookup key ttags
