{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module App where

import Base
import qualified Build
import qualified Defaults
import qualified Files
import qualified Pages
import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util

import qualified X.DemoSimplest (registerX)
import qualified X.DemoCfgArgs (registerX)
import qualified X.PageAnchors (registerX)
import qualified X.Image (registerX)
import qualified X.Links (registerX)
import qualified X.Repeat (registerX)
import qualified X.Snippet (registerX)
import qualified X.MiniTag (registerX)

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import qualified System.IO
import qualified Text.Printf


xregs = [ "hax/demoSimplest" =: X.DemoSimplest.registerX
        , "hax/demoCfgArgs" =: X.DemoCfgArgs.registerX
        , "hax/pageAnchors" =: X.PageAnchors.registerX
        , "hax/image" =: X.Image.registerX
        , "hax/links" =: X.Links.registerX
        , "hax/repeat" =: X.Repeat.registerX
        , "hax/snippet" =: X.Snippet.registerX
        , "hax/miniTag" =: X.MiniTag.registerX
        ]


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

    -> Text.Printf.printf "\n3/6\tCopying %u/%u file(s) to:\n\t~>\t%s\n" numcopyfiles (numcopyfiles+numskipfiles) dirbuild
    >> System.IO.hFlush System.IO.stdout
    >> Build.copyStaticFiles buildplan
    >> Data.Time.Clock.getCurrentTime >>= \timecopydone

    -> Text.Printf.printf "\n4/6\tGenerating %u/%u file(s) in:\n\t~>\t%s\n" numgenpages (numgenpages+numskippages) dirbuild
    >> System.IO.hFlush System.IO.stdout
    >> Pages.processAll ctxmain ctxproj buildplan >>= \(warnpages , ctxbuild)
    -> Data.Time.Clock.getCurrentTime >>= \timeprocdone

    -> Text.Printf.printf "\n5/6\tWriting %u/%u XML file(s) to:\n\t~>\t%s\n" numxmls (numxmls+numskipposts) dirbuild
    >> Pages.writeSitemapXml ctxproj buildplan
    >> Posts.writeAtoms ctxbuild (ctxproj-:Proj.domainName) (buildplan-:Build.feedJobs)
    >> Data.Time.Clock.getCurrentTime >>= \timexmldone

    -> let
        deploymsg = Text.Printf.printf "\n6/6\tCopying only the %u newly (over)written file(s) also to:\n\t~>\t" numoutfiles
        doordonot = if null$ ctxproj-:Proj.dirPathDeploy
                    then putStrLn (deploymsg++ "(skipping this step.)")
                    else putStrLn (deploymsg++(ctxproj-:Proj.dirPathDeploy))
                        >> System.IO.hFlush System.IO.stdout
                        >> Build.copyAllOutputsToDeploy buildplan
    in Util.via doordonot (buildplan , warnpages , numoutfiles , numxmls , timeinitdone , timecopydone , timeprocdone, timexmldone)
