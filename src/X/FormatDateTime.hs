module X.FormatDateTime where

import Base
import qualified Html
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
    str2utc str = ProjC.dtStr2Utc projcfg dtfnfrom str
    str2utc' str = ProjC.dtStr2Utc projcfg "" str
    utc2str utc = ProjC.dtUtc2Str projcfg dtfnto utc

    (dtfnfrom , dtfnto) = xreg-:X.cfgSplitOnce
