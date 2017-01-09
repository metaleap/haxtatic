#!/usr/bin/env stack
{-stack --install-ghc runghc
-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Base
import qualified Build
import qualified Defaults
import qualified Files
import qualified Pages
import qualified Posts
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
    -> putStrLn "\n\n\n==== HAXTATIC ====\n"
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

        -- SHOW TIME!
        in processAll ctxmain projfilename (drop 2 cmdargs)

        >>= \(timeinitdone , timecopydone , timeprocdone , timexmldone)
        -> Data.Time.Clock.getCurrentTime >>= \endtime
        -> let
            timetaken = Util.duration starttime endtime
            showtime difftime = let overaminute = difftime > 60
                                in (overaminute |? (difftime / 60) |! difftime)
                                ~> show ~> ( Util.cropOn1st '.' 3 ['0'] (++(overaminute |? "m" |! "s")) )
        in putStrLn ("\n\nWell it's been " ++(showtime timetaken)++ ":")
        >> putStrLn ("\t"++(showtime$ Util.duration starttime timeinitdone)++ " initializing, pre-templating, planning")
        >> putStrLn ("\t"++(showtime$ Util.duration timecopydone timeprocdone)++ " page templating & generation")
        >> putStrLn ("\t"++(showtime$ Util.duration timeprocdone timexmldone)++ " XML (atoms, sitemap) file generation")
        >> putStrLn ("\t"++(showtime$ ((Util.duration timeinitdone timecopydone) + (Util.duration timeprocdone endtime)))++ " file-copying")
        >> putStrLn ("\n\n==== Bye now! ====\n\n\n")
        >> System.IO.hFlush System.IO.stdout



processAll ctxmain projfilename custfilenames =
    let dirpath = ctxmain.:Files.dirPath
        filenameonly = System.FilePath.takeFileName -- turn a mistakenly supplied file-path back into just-name

    in putStrLn "\n1/6\tReading essential project files [or (re)creating them..]"
    >> System.IO.hFlush System.IO.stdout
    >> System.Directory.createDirectoryIfMissing False dirpath
    >> System.Directory.makeAbsolute dirpath >>= \dirfullpath   --  we do this just in case
    -> let projname = System.FilePath.takeBaseName dirfullpath  --  `dirpath` ended in `.` or `..`
    in Defaults.loadOrCreate ctxmain projname (projfilename~>filenameonly) (custfilenames>~filenameonly)
    >>= Proj.loadCtx ctxmain projname xregs >>= \ctxproj
    -> Tmpl.warnIfTagMismatches ctxmain "*.haxproj"
                (ctxproj.:Proj.setup.:Proj.tagMismatches)

    >> putStrLn ("\n2/6\tPlanning the work..")
    >> System.IO.hFlush System.IO.stdout
    >> Build.plan ctxmain ctxproj >>= \buildplan
    -> let
        numgenpages = buildplan.:Build.outPages~>length
        numdynpages = buildplan.:Build.numDynPages
        numskippages = buildplan.:Build.numSkippedPages
        numcopyfiles = buildplan.:Build.outStatics~>length
        numskipfiles = buildplan.:Build.numSkippedStatic
        numskipposts = buildplan.:Build.numSkippedAtoms
        numoutfiles = buildplan.:Build.numOutFilesTotal
        numxmlfiles = buildplan.:Build.outAtoms~>length
        numsitemaps = ((buildplan.:Build.siteMap~>fst) == Build.NoOutput) |? 0 |! 1
        dirbuild = ctxproj.:Proj.dirPathBuild
    in putStrLn ("\t->\tStatic files: will copy " ++(show numcopyfiles)++ ", skipping " ++(show numskipfiles)++ "")
    >> putStrLn ("\t->\tContent pages: will (re)generate " ++(show$ numgenpages - numdynpages)++"+"++(show numdynpages)++ ", skipping " ++(show numskippages)++ "")
    >> putStrLn ("\t->\tXML files: will (re)generate from "++(show numxmlfiles)++" feeds, skipping " ++(show numskipposts)++ "\n\t\t           plus "++(show numsitemaps)++" sitemap(s)")
    >> Data.Time.Clock.getCurrentTime >>= \timeinitdone

    -> putStrLn ("\n3/6\tCopying " ++(show numcopyfiles)++ "/" ++(show$ numcopyfiles+numskipfiles)++ " static file(s) to:\n\t->\t"++dirbuild)
    >> System.IO.hFlush System.IO.stdout
    >> Build.copyStaticFiles buildplan
    >> Data.Time.Clock.getCurrentTime >>= \timecopydone

    -> putStrLn ("\n4/6\tGenerating " ++(show numgenpages)++ "/" ++(show$ numgenpages+numskippages)++ " page(s) in:\n\t->\t"++dirbuild)
    >> System.IO.hFlush System.IO.stdout
    >> Pages.processAll ctxmain ctxproj buildplan
    >> Data.Time.Clock.getCurrentTime >>= \timeprocdone
    -> Pages.writeSitemapXml ctxproj buildplan
    >> putStrLn ("\n5/6\tGenerating " ++(show numxmlfiles)++ "/" ++(show$ numxmlfiles+numskipposts)++ " Atom feed(s) in:\n\t->\t"++dirbuild)
    >> Posts.writeAtoms (buildplan.:Build.allPagesFiles) (ctxproj.:Proj.setup.:Proj.bloks)
                            (ctxproj.:Proj.setup.:Proj.posts) (ctxproj.:Proj.setup.:Proj.cfg) (buildplan.:Build.feedJobs)
    >> Data.Time.Clock.getCurrentTime >>= \timexmldone
    -> let
        deploymsg = "\n6/6\tCopying only the " ++(show$ numoutfiles)++ " newly (over)written file(s) also to:\n\t->\t"
        doordonot = if null$ ctxproj.:Proj.dirPathDeploy
                    then putStrLn (deploymsg++ "(skipping this step.)")
                    else putStrLn (deploymsg++(ctxproj.:Proj.dirPathDeploy))
                        >> System.IO.hFlush System.IO.stdout
                        >> Build.copyAllOutputsToDeploy buildplan
    in Util.via doordonot (timeinitdone , timecopydone , timeprocdone, timexmldone)
