#!/usr/bin/env stack
{- stack --install-ghc runghc -}
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Build
import qualified Files
import qualified Proj
import qualified ProjDefaults
import qualified Util
import Util ( (#) , (~:) )

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import qualified System.IO



main ::
    IO ()
main =
    Data.Time.Clock.getCurrentTime >>= \ starttime
    -> System.IO.hFlush System.IO.stdout -- only because SublimeText2 build output is wonky with shell scripts
    >> putStrLn "\n\n\n==== HAXTATIC ====\n"
    >> System.IO.hFlush System.IO.stdout
    >> System.Environment.getArgs >>= \ cmdargs
    -> System.Directory.getCurrentDirectory >>= \ curdir
    -> if null cmdargs
        then putStrLn "No project-directory path supplied.\n\
            \  For existing project: specify path to its current directory.\n\
            \  For a new project: specify path to its intended directory.\n    (I'll create it if missing and its parent isn't.)\n\n"
        else
            let dirpath = cmdargs#0
                ctxmain = Files.Ctx {   Files.curDir = curdir,
                                        Files.dirPath = dirpath,
                                        Files.nowTime=starttime }
            in process ctxmain (Util.atOr cmdargs 1 "default.haxproj") (Util.atOr cmdargs 2 "")
            >> Data.Time.Clock.getCurrentTime >>= \ endtime
            -> let timetaken = Data.Time.Clock.diffUTCTime endtime starttime
            in putStrLn $"\n\nWell it's been "++(show timetaken)++":\n\n==== Bye now! ====\n\n\n"



process ctxmain projfilename custfilename =
    let filename = System.FilePath.takeFileName -- turn a mistakenly supplied file-path back into just-name
        dirpath = ctxmain~:Files.dirPath
        projname = System.FilePath.takeBaseName dirpath

    in putStrLn "1. Reading essential project files [or (re)creating them..]"
    >> System.IO.hFlush System.IO.stdout
    >> System.Directory.createDirectoryIfMissing False dirpath
    >> ProjDefaults.loadOrCreate ctxmain projname (filename projfilename) (filename custfilename)
    >>= Proj.loadCtx ctxmain projname >>= \ ctxproj

    -> putStrLn "2. Scanning input files and folders.."
    >> System.IO.hFlush System.IO.stdout
    >> Build.plan ctxmain ctxproj >>= \ buildplan
    -> putStrLn ("\t->\tStatic files: will copy "++(show$ buildplan~:Build.outFilesStatic~:length)++" (skipping "++(show$ buildplan~:Build.numSkippedStatic)++")")
    >> putStrLn ("\t->\tContent pages: will (re)generate "++(show$ buildplan~:Build.outFilesPage~:length)++" (skipping "++(show$ buildplan~:Build.numSkippedPages)++")")
    >> putStrLn ("\t->\tAtom XML files: will (re)generate "++(show$ buildplan~:Build.outFilesAtom~:length)++" (skipping "++(show$ buildplan~:Build.numSkippedAtoms)++")")
    --  >> print buildplan

    >> putStrLn ("3. Copying "++(show$ buildplan~:Build.outFilesStatic~:length)++"/"++(show$ buildplan~:Build.outFilesStatic~:length + buildplan~:Build.numSkippedStatic)++" static files..")
    >> System.IO.hFlush System.IO.stdout
    >> Build.copyStaticFiles buildplan

    >> processCopyDeploy ctxproj buildplan




processCopyDeploy ctxproj buildplan =
    if null (ctxproj~:Proj.dirPathDeploy)
        then
            return ()
        else
            putStrLn ("4. Copying all "++(show$ buildplan~:Build.numOutFilesTotal)++" output files written to `"++(ctxproj~:Proj.dirPathDeploy)++"` ..")
            >> System.IO.hFlush System.IO.stdout
            >> Build.copyAllOutputsToDeploy buildplan
