{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module ProjT where

import Hax.Base

import qualified Util

import qualified Data.Map.Strict


parseProjChunks chunkssplits =
    Data.Map.Strict.fromList$ chunkssplits ~> Util.keepNoNilFsts foreach where
        foreach (tname:tvalsplits) =
            ( tname ~> Util.trim ,
                tvalsplits ~> (Util.join ":") ~> Util.trim )
        foreach _ =
            ( "" , "" )


tagHandler ttags key =
    Data.Map.Strict.lookup key ttags
