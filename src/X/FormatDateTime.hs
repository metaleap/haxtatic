{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.FormatDateTime where

import Base
import qualified Proj
import qualified ProjC
import qualified X



registerX ctxproj xreg =
    X.Early (dtformatter . snd)
    where
    dtformatter = dtFormatter ctxproj dtfnfrom dtfnto
    (dtfnfrom , dtfnto) = xreg-:X.cfgSplitOnce


dtFormatter ctxproj dtfnfrom dtfnto =
    roundtrip
    where
    roundtrip dt =
        (str2utc dt <|> str2utc' dt) >~ utc2str
    str2utc = ProjC.dtStr2Utc projcfg dtfnfrom
    str2utc' = ProjC.dtStr2Utc projcfg ""
    utc2str = ProjC.dtUtc2Str projcfg dtfnto
    projcfg = ctxproj-:Proj.setup-:Proj.cfg
