module App (processAll)
where

--  writing an X-renderer? register it in `xregs` right below these imports

import Base

import qualified Build
import qualified Defaults
import qualified Files
import qualified Pages
import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import qualified System.IO
import qualified Text.Printf

import qualified X.DemoSimplest (registerX)
import qualified X.DemoCfgArgs (registerX)
import qualified X.FeedView (registerX)
import qualified X.FormatDateTime (registerX)
import qualified X.HtmlAnchors (registerX)
import qualified X.HtmlImage (registerX)
import qualified X.HtmlLink (registerX)
import qualified X.HtmlLinks (registerX)
import qualified X.NoOp (registerX)
import qualified X.Iterator (registerX)
import qualified X.Snippet (registerX)
import qualified X.MiniTag (registerX)
import qualified X.UnMarkup (registerX)
import qualified X.XmlEscape (registerX)


xregs = [ "demoSimplest" =: X.DemoSimplest.registerX
        , "demoCfgArgs" =: X.DemoCfgArgs.registerX
        , "hax.dtFormat" =: X.FormatDateTime.registerX
        , "hax.feedView" =: X.FeedView.registerX
        , "hax.htmlAnchors" =: X.HtmlAnchors.registerX
        , "hax.htmlImage" =: X.HtmlImage.registerX
        , "hax.htmlLink" =: X.HtmlLink.registerX
        , "hax.htmlLinks" =: X.HtmlLinks.registerX
        , "hax.noOp" =: X.NoOp.registerX
        , "hax.iterator" =: X.Iterator.registerX
        , "hax.snippet" =: X.Snippet.registerX
        , "hax.miniTag" =: X.MiniTag.registerX
        , "hax.unMarkup" =: X.UnMarkup.registerX
        , "hax.xmlEscape" =: X.XmlEscape.registerX
        ]


processAll ctxmain projfilename custfilenames =
    let nameonly = System.FilePath.takeFileName -- turn a mistakenly supplied file-path back into just-name
        projname = nameonly (ctxmain-:Files.dirPath)
    in putStrLn "\n1/6\tReading essential project files (or creating them).."
    *> System.IO.hFlush System.IO.stdout
    *> System.Directory.createDirectoryIfMissing False (ctxmain-:Files.dirPath)
    *> Defaults.loadOrCreate ctxmain projname (projfilename~>nameonly) (custfilenames>~nameonly)
    >>= Proj.loadCtx ctxmain projname xregs >>= \ ctxproj
    -> Tmpl.warnIfTagMismatches ctxmain "*.haxproj (or *.haxsnip)"
                (ctxproj-:Proj.setup-:Proj.tagMismatches)

    *> putStrLn ("\n2/6\tPlanning the work..")
    *> System.IO.hFlush System.IO.stdout
    *> Build.plan ctxmain ctxproj >>= \ buildplan
    -> let
        numdynpages = buildplan-:Build.numDynPages ; numskippages = buildplan-:Build.numSkippedPages
        numskipfiles = buildplan-:Build.numSkippedStatic ; numskipposts = buildplan-:Build.numSkippedAtoms
        numoutfiles = buildplan-:Build.numOutFilesTotal ; numxmls = numatoms + numsitemaps
        numgenpages = buildplan-:Build.outPages~>length
        numcopyfiles = buildplan-:Build.outStatics~>length
        numatoms = buildplan-:Build.outAtoms~>length
        numsitemaps = ((buildplan-:Build.siteMap~>fst) == Build.NoOutput) |? 0 |! 1
        dirbuild = ctxproj-:Proj.dirPathBuild
    in Text.Printf.printf "\t->\tStatic files: will copy %u, skipping %u\n" numcopyfiles numskipfiles
    *> Text.Printf.printf "\t->\tContent pages: will generate %u+%u, skipping %u\n" (numgenpages - numdynpages) numdynpages numskippages
    *> Text.Printf.printf "\t->\tXML files: will generate %u feeds, skipping %u\n\t\t           plus %u sitemap(s)\n" numatoms numskipposts numsitemaps
    *> Data.Time.Clock.getCurrentTime >>= \timeinitdone

    -> Text.Printf.printf "\n3/6\tCopying %u/%u file(s) to:\n\t~>\t%s\n" numcopyfiles (numcopyfiles+numskipfiles) dirbuild
    *> System.IO.hFlush System.IO.stdout
    *> Build.copyStaticFiles buildplan
    *> Data.Time.Clock.getCurrentTime >>= \timecopydone

    -> Text.Printf.printf "\n4/6\tGenerating %u/%u file(s) in:\n\t~>\t%s\n" numgenpages (numgenpages+numskippages) dirbuild
    *> System.IO.hFlush System.IO.stdout
    *> Pages.processAll ctxmain ctxproj buildplan >>= \(warnpages , hintpages , ctxbuild)
    -> Data.Time.Clock.getCurrentTime >>= \timeprocdone

    -> Text.Printf.printf "\n5/6\tWriting %u/%u XML file(s) to:\n\t~>\t%s\n" numxmls (numxmls+numskipposts) dirbuild
    *> Pages.writeSitemapXml ctxproj buildplan
    *> Posts.writeAtoms ctxbuild (ctxproj-:Proj.domainName) (buildplan-:Build.feedJobs)
    *> Data.Time.Clock.getCurrentTime >>= \timexmldone

    -> let
        extrahints = if (ctxproj-:Proj.setup-:Proj.tagMismatches~>fst /= ctxproj-:Proj.setup-:Proj.tagMismatches~>snd)
                        then ["*.haxproj or *.haxsnip"] else []
        deploymsg = Text.Printf.printf "\n6/6\tCopying only the %u newly (over)written file(s) also to:\n\t~>\t" numoutfiles
        doordonot = if null$ ctxproj-:Proj.dirPathDeploy
                    then putStrLn (deploymsg ++ "(skipping this step.)")
                    else putStrLn (deploymsg ++ (ctxproj-:Proj.dirPathDeploy))
                        *> System.IO.hFlush System.IO.stdout
                        *> Build.copyAllOutputsToDeploy buildplan
    in Util.via doordonot (buildplan , warnpages , hintpages ++ extrahints , numoutfiles , numxmls , timeinitdone , timecopydone , timeprocdone, timexmldone)
