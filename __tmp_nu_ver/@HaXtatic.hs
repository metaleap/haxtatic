{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Proj
import qualified ProjDefaults
import qualified Util
import Util ( (#) , (°) )

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import System.FilePath ( (</>) )



main ::
    IO ()
main = do
    starttime <- Data.Time.Clock.getCurrentTime
    cmdargs <- System.Environment.getArgs
    putStrLn "\n\n\n==== HAXTATIC ====\n"
    let numargs = (°)cmdargs in
        if 0==numargs then putStrLn "\
            \No project-directory path supplied.\n  For existing project: specify path to its directory.\n\
            \  For a new project: specify its target directory (existing or not).\n\n"
        else
            process (cmdargs#0) (Util.atOr cmdargs 1 "default.haxproj") (Util.atOr cmdargs 2 "")
            >> Data.Time.Clock.getCurrentTime
            >>= \nowtime -> let timetaken = Data.Time.Clock.diffUTCTime nowtime starttime in
                putStrLn $ "\n\nBYE NOW! It's been "++(show timetaken)++".\n\n\n"



process dirpath projfilename custfilename = let
    in
        System.Directory.createDirectoryIfMissing False dirpath
        >> ProjDefaults.loadOrCreate dirpath projfilename custfilename
        >>= Proj.loadCtx dirpath
        >>= print
