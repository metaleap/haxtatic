#!/usr/bin/env stack
{-stack --install-ghc runghc
-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
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
import qualified XpageAnchors
import qualified Ximage
import qualified Xlinks
import qualified Xrepeat
import qualified Xsnippet
import qualified XminiTag

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import qualified System.IO
import qualified Text.Printf


xregs = [ "hax/demoSimplest" =: XdemoSimplest.registerX
        , "hax/demoCfgArgs" =: XdemoCfgArgs.registerX
        , "hax/pageAnchors" =: XpageAnchors.registerX
        , "hax/image" =: Ximage.registerX
        , "hax/links" =: Xlinks.registerX
        , "hax/repeat" =: Xrepeat.registerX
        , "hax/snippet" =: Xsnippet.registerX
        , "hax/miniTag" =: XminiTag.registerX
        ]


main ::
    IO ()
main =
    Data.Time.Clock.getCurrentTime >>= \starttime
    -> putStrLn "\n\n==== HAXTATIC ====\n"
    >> System.Environment.getArgs >>= \cmdargs
    -> System.Directory.getCurrentDirectory >>= \curdir
    -> if null cmdargs
        then putStrLn "No project-directory path supplied.\n\
            \  For existing project: specify path to its current directory.\n\
            \  For a new project: specify path to its intended directory.\n    (I'll create it if missing and its parent isn't.)\n\n"
        else
        let projfilename = (Defaults.fileName_Proj -|= cmdargs@?1)
        in System.Directory.makeAbsolute (cmdargs@!0) >>= \dirpath
        -> let ctxmain = Files.AppContext { Files.curDir = curdir,
                                            Files.dirPath = dirpath,
                                            Files.setupName = Defaults.setupName projfilename,
                                            Files.nowTime=starttime }
        --  GET TO WORK:
        in processAll ctxmain projfilename (drop 2 cmdargs)
        --  REMINISCE:

        >>= \(buildplan , warnpages , numoutfiles , numxmls , timeinitdone , timecopydone , timeprocdone , timexmldone)
        -> Data.Time.Clock.getCurrentTime >>= \endtime
        -> let
            timetaken = Util.duration starttime endtime
            showtime = showtime' 3
            showtime' numdig difftime =
                let overaminute = difftime > 60
                in (overaminute |? (difftime / 60) |! difftime)
                    ~> show ~> ( Util.cropOn1st '.' numdig ['0'] (++(overaminute |? "m" |! "s")) )
            showavg 0 _ _ = ""
            showavg l t1 t2 = Text.Printf.printf " (%ux ~%s)" l (showtime' 4 ((Util.duration t1 t2) / (fromIntegral l)))
        in Text.Printf.printf "\n\nWrote %u files in %s:" numoutfiles (showtime timetaken)
        >> Text.Printf.printf "\n\t%s pre-templating & planning"
                                (showtime$ Util.duration starttime timeinitdone)
        >> Text.Printf.printf "\n\t%s page templating & generation%s"
                                (showtime$ Util.duration timecopydone timeprocdone)
                                (showavg (buildplan-:Build.outPages~>length) timecopydone timeprocdone)
        >> Text.Printf.printf "\n\t%s XML file generation%s"
                                (showtime$ Util.duration timeprocdone timexmldone)
                                (showavg numxmls timeprocdone timexmldone)
        >> Text.Printf.printf "\n\t%s misc. & file-copying%s"
                                (showtime$ ((Util.duration timeinitdone timecopydone) + (Util.duration timexmldone endtime)))
                                (showavg (buildplan-:Build.outStatics~>length) timeprocdone endtime)
        >> if null warnpages
            then putStrLn ("\n\n==== Bye now! ====\n\n")
            else putStrLn ("\n\n" ++ (Util.join "\n\t!>\t" ("Apparent {!| ERROR MESSAGES |!} were rendered into:":warnpages)) ++ "\n\n")
        >> System.IO.hFlush System.IO.stdout



processAll ctxmain projfilename custfilenames =
    let nameonly = System.FilePath.takeFileName -- turn a mistakenly supplied file-path back into just-name

    in putStrLn "\n1/6\tReading essential project files (or creating them).."
    >> System.IO.hFlush System.IO.stdout
    >> let projname = System.FilePath.takeBaseName (ctxmain-:Files.dirPath)
    in System.Directory.createDirectoryIfMissing False (ctxmain-:Files.dirPath)
    >> Defaults.loadOrCreate ctxmain projname (projfilename~>nameonly) (custfilenames>~nameonly)
    >>= Proj.loadCtx ctxmain projname xregs >>= \ctxproj
    -> Tmpl.warnIfTagMismatches ctxmain "*.haxproj"
                (ctxproj-:Proj.setup-:Proj.tagMismatches)

    >> putStrLn ("\n2/6\tPlanning the work..")
    >> System.IO.hFlush System.IO.stdout
    >> Build.plan ctxmain ctxproj >>= \buildplan
    -> let
        numgenpages = buildplan-:Build.outPages~>length
        numdynpages = buildplan-:Build.numDynPages
        numskippages = buildplan-:Build.numSkippedPages
        numcopyfiles = buildplan-:Build.outStatics~>length
        numskipfiles = buildplan-:Build.numSkippedStatic
        numskipposts = buildplan-:Build.numSkippedAtoms
        numoutfiles = buildplan-:Build.numOutFilesTotal
        numatoms = buildplan-:Build.outAtoms~>length
        numsitemaps = ((buildplan-:Build.siteMap~>fst) == Build.NoOutput) |? 0 |! 1
        numxmls = numatoms + numsitemaps
        dirbuild = ctxproj-:Proj.dirPathBuild
    in Text.Printf.printf "\t->\tStatic files: will copy %u, skipping %u\n" numcopyfiles numskipfiles
    >> Text.Printf.printf "\t->\tContent pages: will generate %u+%u, skipping %u\n" (numgenpages - numdynpages) numdynpages numskippages
    >> Text.Printf.printf "\t->\tXML files: will generate %u feeds, skipping %u\n\t\t           plus %u sitemap(s)\n" numatoms numskipposts numsitemaps
    >> Data.Time.Clock.getCurrentTime >>= \timeinitdone

    -> Text.Printf.printf "\n3/6\tCopying %u/%u static file(s) to:\n\t->\t%s\n" numcopyfiles (numcopyfiles+numskipfiles) dirbuild
    >> System.IO.hFlush System.IO.stdout
    >> Build.copyStaticFiles buildplan
    >> Data.Time.Clock.getCurrentTime >>= \timecopydone

    -> Text.Printf.printf "\n4/6\tGenerating %u/%u page(s) in:\n\t->\t%s\n" numgenpages (numgenpages+numskippages) dirbuild
    >> System.IO.hFlush System.IO.stdout
    >> Pages.processAll ctxmain ctxproj buildplan >>= \(warnpages , pagerendercache)
    -> Data.Time.Clock.getCurrentTime >>= \timeprocdone

    -> Text.Printf.printf "\n5/6\tGenerating %u/%u XML files in:\n\t->\t%s\n" numxmls (numxmls+numskipposts) dirbuild
    >> Pages.writeSitemapXml ctxproj buildplan
    >> Posts.writeAtoms (Posts.BuildContext (Just pagerendercache)
                                            (buildplan-:Build.allPagesFiles)
                                            (ctxproj-:Proj.setup-:Proj.bloks)
                                            (ctxproj-:Proj.setup-:Proj.posts)
                                            (ctxproj-:Proj.setup-:Proj.cfg))
                        (buildplan-:Build.feedJobs)
    >> Data.Time.Clock.getCurrentTime >>= \timexmldone

    -> let
        deploymsg = Text.Printf.printf "\n6/6\tCopying only the %u newly (over)written file(s) also to:\n\t->\t" numoutfiles
        doordonot = if null$ ctxproj-:Proj.dirPathDeploy
                    then putStrLn (deploymsg++ "(skipping this step.)")
                    else putStrLn (deploymsg++(ctxproj-:Proj.dirPathDeploy))
                        >> System.IO.hFlush System.IO.stdout
                        >> Build.copyAllOutputsToDeploy buildplan
    in Util.via doordonot (buildplan , warnpages , numoutfiles , numxmls , timeinitdone , timecopydone , timeprocdone, timexmldone)
