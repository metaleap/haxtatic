{-# OPTIONS_GHC -Wall #-}

module ProjCfg where

import qualified ProjDefaults
import qualified Util
import Util ( (~>) , (>~) , (~|) , (|~) , (~.) )

import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Text.Read


data Cfg = Cfg {
    outDirs :: [String],
    processStatic :: Processing,
    processPages :: Processing,
    processPosts :: Processing
} deriving (Read, Show)

data Processing = Processing {
    skip :: [String],
    force :: [String],
    dirs :: [String]
} deriving (Read, Show)



parseDefs linessplits =
    Cfg { outDirs = outdirs, processStatic=procstatic, processPages=procpages, processPosts=procposts }
    where
        procstatic = procfind ProjDefaults.processingDir_Static
        procpages = procfind ProjDefaults.processingDir_Pages
        procposts = procfind ProjDefaults.processingDir_Posts
        procfind name = procsane name $ Data.Maybe.fromMaybe (procdef name) $
                        Data.Map.Strict.findWithDefault Nothing ("process:"++name) configs
        procdef dirname = Processing { dirs = [dirname], skip = [], force = [] }
        procsane defname proc = Processing {
                dirs = Util.fallback (proc~>dirs >~Util.trim ~|Util.is) [defname],
                skip = sane skip,
                force = sane force
            } where
                sane fvals = let tmp = proc~>fvals >~Util.trim ~|Util.is in
                    if elem "*" tmp then ["*"] else tmp
        outdirs = if null tmp then [ProjDefaults.dir_Out] else take 2 $tmp~>last where
            tmp = linessplits>~persplit ~|Util.is
            persplit ("C":"":"out":"dirs":outdirs) = outdirs >~Util.trim ~|(Util.isBut ".")
            persplit _ = []
        configs = Data.Map.Strict.fromList$
            linessplits>~persplit ~|Util.is.fst where
            persplit ("C":"":"process":name:procstr) =
                ( "process:"++name ,
                    (Text.Read.readMaybe$ "Processing {"++(Util.join ":" procstr)++"}") :: Maybe Processing)
            persplit _ =
                ( "" , Nothing )
