{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.FormatDateTime where

import Base
import qualified Proj
import qualified ProjC
import qualified X



registerX ctxproj xreg =
    let
    renderer (_ , argstr) =
        (str2utc argstr <|> str2utc' argstr) >~ utc2str

    in X.Early renderer
    where

    projcfg = ctxproj-:Proj.setup-:Proj.cfg
    str2utc = ProjC.dtStr2Utc projcfg dtfnfrom
    str2utc' = ProjC.dtStr2Utc projcfg ""
    utc2str = ProjC.dtUtc2Str projcfg dtfnto

    (dtfnfrom , dtfnto) = xreg-:X.cfgSplitOnce
