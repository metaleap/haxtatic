#!/usr/bin/env stack
{- stack --install-ghc runghc -}
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Build
import qualified Files
import qualified Proj
import qualified ProjDefaults
import qualified Util
import Util ( (#) , (~>) , (~.) )

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
    -> if null cmdargs
        then putStrLn "No project-directory path supplied.\n\
            \  For existing project: specify path to its current directory.\n\
            \  For a new project: specify path to its intended directory.\n    (I'll create it if missing and its parent isn't.)\n\n"
        else
            let dirpath = cmdargs#0
                ctxmain = Files.Ctx { Files.dirPath = dirpath , Files.nowTime=starttime }
            in process ctxmain (Util.atOr cmdargs 1 "default.haxproj") (Util.atOr cmdargs 2 "")
            >> Data.Time.Clock.getCurrentTime >>= \ endtime
            -> let timetaken = Data.Time.Clock.diffUTCTime endtime starttime
            in putStrLn $"\n\nWell it's been "++(show timetaken)++":\n\n==== Bye now! ====\n\n\n"



process ctxmain projfilename custfilename =
    let ensurefilename = System.FilePath.takeFileName -- turn a mistakenly supplied file-path back into just-name
        dirpath = ctxmain~>Files.dirPath
        projname = System.FilePath.takeBaseName dirpath

    in putStrLn "1. Reading essential project files [or (re)creating them..]"
    >> System.Directory.createDirectoryIfMissing False dirpath
    >> ProjDefaults.loadOrCreate ctxmain projname (ensurefilename projfilename) (ensurefilename custfilename)
    >>= Proj.loadCtx ctxmain projname >>= \ ctxproj

    -> putStrLn "2. Scanning input files and folders.."
    >> Build.plan ctxproj >>= \ buildplan

    -> print (buildplan~>Build.fileCopies)
    >> print (buildplan~>Build.fileGens)
    --  >> print (ctxproj~>Proj.outDirPaths)
    --  >> putStrLn ("=================")
    --  >> putStrLn (ctxproj~>Proj.coreFiles~>ProjDefaults.htmlTemplateMain~>Files.content)
    --  >> putStrLn ("=================")
    --  >> putStrLn (ctxproj~>Proj.coreFiles~>ProjDefaults.htmlTemplateBlok~>Files.content)
