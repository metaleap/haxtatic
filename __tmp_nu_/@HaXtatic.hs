#!/usr/bin/env stack
{- stack --install-ghc runghc -}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Base
import qualified Build
import qualified Defaults
import qualified Files
import qualified Pages
import qualified Proj
import qualified Tmpl
import qualified Util

import qualified XdemoSimplest
import qualified XdemoCfgArgs
import qualified Ximage
import qualified Xlinks
import qualified XminiTag

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import qualified System.IO



xregs = [ ("hax.demoSimplest" =: XdemoSimplest.registerX)
        , ("hax.demoCfgArgs" =: XdemoCfgArgs.registerX)
        , ("hax.image" =: Ximage.registerX)
        , ("hax.links" =: Xlinks.registerX)
        , ("hax.miniTag" =: XminiTag.registerX)
        ]



main ::
    IO ()
main =
    Data.Time.Clock.getCurrentTime >>= \starttime
    -> System.IO.hFlush System.IO.stdout -- only because SublimeText2 build-output console is wonky with shell scripts
    >> putStrLn "\n\n\n==== HAXTATIC ====\n"
    >> System.Environment.getArgs >>= \cmdargs
    -> System.Directory.getCurrentDirectory >>= \curdir
    -> if null cmdargs
        then putStrLn "No project-directory path supplied.\n\
            \  For existing project: specify path to its current directory.\n\
            \  For a new project: specify path to its intended directory.\n    (I'll create it if missing and its parent isn't.)\n\n"
        else
        let dirpath = cmdargs#0
            projfilename = (Util.atOr cmdargs 1 Defaults.fileName_Proj)
            ctxmain = Files.AppContext {    Files.curDir = curdir,
                                            Files.dirPath = dirpath,
                                            Files.setupName = Defaults.setupName projfilename,
                                            Files.nowTime=starttime }
        in processAll ctxmain projfilename (drop 2 cmdargs)
        >> Data.Time.Clock.getCurrentTime >>= \endtime
        -> let timetaken = Data.Time.Clock.diffUTCTime endtime starttime
        in putStrLn ("\n\nWell it's been " ++(show timetaken)++ ":\n\n==== Bye now! ====\n\n\n")
        >> System.IO.hFlush System.IO.stdout -- seems to force SublimeText2 build-output console to scroll down



processAll ::
    Files.Ctx-> String-> [String]->
    IO ()
processAll ctxmain projfilename custfilenames =
    let dirpath = ctxmain~:Files.dirPath
        filenameonly = System.FilePath.takeFileName -- turn a mistakenly supplied file-path back into just-name

    in putStrLn "\n1/5\tReading essential project files [or (re)creating them..]"
    >> System.Directory.createDirectoryIfMissing False dirpath
    >> System.Directory.makeAbsolute dirpath >>= \dirfullpath   --  we do this just in case
    -> let projname = System.FilePath.takeBaseName dirfullpath  --  `dirpath` ended in `.` or `..`
    in Defaults.loadOrCreate ctxmain projname (projfilename~>filenameonly) (custfilenames>~filenameonly)
    >>= Proj.loadCtx ctxmain projname xregs >>= \ctxproj
    -> Tmpl.warnIfTagMismatches ctxmain "*.haxproj"
                (ctxproj~:Proj.setup~:Proj.tagMismatches)

    >> putStrLn "\n2/5\tPlanning the work.."
    >> Build.plan ctxmain ctxproj >>= \buildplan
    -> let
        numgenpages = buildplan~:Build.outPages~>length
        numskippages = buildplan~:Build.numSkippedPages
        numcopyfiles = buildplan~:Build.outStatics~>length
        numskipfiles = buildplan~:Build.numSkippedStatic
        numoutfiles = buildplan~:Build.numOutFilesTotal
        numxmlfiles = buildplan~:Build.outAtoms~>length
        numsitemaps = ((buildplan~:Build.siteMap~>fst) == Build.NoOutput) |? 0 |! 1
        dirbuild = ctxproj~:Proj.dirPathBuild
    in putStrLn ("\t->\tStatic files: will copy " ++(show numcopyfiles)++ ", skipping " ++(show numskipfiles)++ "")
    >> putStrLn ("\t->\tContent pages: will (re)generate from " ++(show numgenpages)++ ", skipping " ++(show numskippages)++ "")
    >> putStrLn ("\t->\tXML files: will (re)generate from "++(show numxmlfiles)++" feeds, skipping " ++(show$ buildplan~:Build.numSkippedAtoms)++ "\n\t\t           plus "++(show numsitemaps)++" sitemap(s)")

    >> putStrLn ("\n3/5\tCopying " ++(show numcopyfiles)++ "/" ++(show$ numcopyfiles+numskipfiles)++ " static file(s) to:\n\t->\t"++dirbuild)
    >> Build.copyStaticFiles buildplan

    >> putStrLn ("\n4/5\tGenerating " ++(show numgenpages)++ "/" ++(show$ numgenpages+numskippages)++ " page(s) in:\n\t->\t"++dirbuild)
    >> Pages.processAll ctxmain ctxproj buildplan
    >> Pages.writeSitemapXml ctxproj buildplan

    --  >> let  dtstr = (Proj.dtUtc2Str (ctxproj~:Proj.setup~:Proj.cfg) "foo" (ctxmain~:Files.nowTime))
    --          dtutc = Proj.dtStr2UtcOr (ctxproj~:Proj.setup~:Proj.cfg) "foo" dtstr Util.dateTime0
    --  in print dtstr
    --  >> print dtutc
    >> System.IO.hFlush System.IO.stdout -- remove later

    >> let deploymsg = "\n5/5\tCopying only the " ++(show$ numoutfiles)++ " newly (over)written file(s) also to:\n\t->\t"
    in if null (ctxproj~:Proj.dirPathDeploy) then
        putStrLn (deploymsg++ "(skipping this step.)") else
            putStrLn (deploymsg++(ctxproj~:Proj.dirPathDeploy))
            >> Build.copyAllOutputsToDeploy buildplan
