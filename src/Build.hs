module Build where

import Hax.Base

import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Posts
import qualified Proj
import qualified ProjC
import qualified Util

import qualified Data.List
import qualified Data.Time.Clock
import qualified System.Directory
import System.FilePath ( (</>) )



data Plan
    = BuildPlan {
        outAtoms :: [Task],
        outPages :: [Task],
        outStatics :: [Task],
        numOutFilesTotal :: Int,
        numDynPages :: Int,
        numSkippedStatic :: Int,
        numSkippedPages :: Int,
        numSkippedAtoms :: Int,
        anyProcessing :: Bool,
        siteMap :: (Task , [Task]),
        allPagesFiles :: [(FilePath , Files.File)],
        feedJobs :: [Posts.Feed]
    }


data Task
    = NoOutput
    | FileOutput {
        relPath :: FilePath,
        relPathSlashes :: FilePath,
        blokName :: String,
        outPathBuild :: FilePath,
        outPathDeploy :: FilePath,
        projPathCached :: FilePath,
        contentDate :: Data.Time.Clock.UTCTime,
        srcFile :: Files.File
    }
    deriving Eq



copyAllOutputsToDeploy buildplan =
    let foreach NoOutput =
            pure ()
        foreach builtfile =
            let srcfilepath = builtfile-:outPathBuild
                ifexists True =
                    Files.copyTo srcfilepath [builtfile-:outPathDeploy]
                ifexists False =
                    putStrLn ("\t-!\tMissing: `" ++srcfilepath++ "`")
            in System.Directory.doesFileExist srcfilepath >>= ifexists
    in (buildplan-:outStatics) >>~ foreach
    *> (buildplan-:outPages) >>~ foreach
    *> (buildplan-:outAtoms) >>~ foreach
    *> (buildplan-:siteMap~>fst) ~> foreach
    *> pure ()



copyStaticFiles buildplan =
    (buildplan-:outStatics) >>~ foreach where
        foreach file =
            Files.copyTo (file-:srcFile-:Files.path) [file-:outPathBuild]



plan ctxmain ctxproj =
    let createindexhtmlifnocontentpages numpagesrcfiles =
            if numpagesrcfiles > 0 then pure NoOutput
            else let
                sitename = ctxproj-:Proj.projName
                dirpagesrel = (ctxproj-:Proj.setup-:Proj.cfg-:ProjC.processingOfPages-:ProjC.dirs)@!0
                dirbuild = ctxproj-:Proj.dirPathBuild
                htmltemplatemain = ctxproj-:Proj.coreFiles-:Defaults.htmlTemplateMain
            in putStrLn ("\t->\tNo content-source files whatsoever.. making one for you:")
            *> Defaults.writeDefaultIndexHtml
                ctxmain sitename dirpagesrel dirbuild htmltemplatemain
            >>= \(srcfile , relpath , outjobpath)
            -> pure FileOutput {
                            relPath = relpath,
                            relPathSlashes = Files.pathSepSystemToSlash relpath,
                            blokName = "",
                            outPathBuild = outjobpath,
                            outPathDeploy = Util.ifIs (ctxproj-:Proj.dirPathDeploy) (</> relpath),
                            projPathCached = Util.ifIs (ctxproj-:Proj.dirPathCache) (</> relpath),
                            contentDate = srcfile-:Files.modTime,
                            srcFile = srcfile
                        }
        projsetup = ctxproj-:Proj.setup
        projcfg = projsetup-:Proj.cfg
        cfgprocstatic = projcfg-:ProjC.processingOfFiles
        cfgprocpages = projcfg-:ProjC.processingOfPages
        cfgprocposts = ProjC.Proc { ProjC.force = Util.onlyIf (cfgprocpages-:ProjC.force) ["*"] [],
                                    ProjC.skip = [], ProjC.dirs = []}
        listallfiles = Files.listAllFiles $ctxproj-:Proj.dirPath
        modtimeproj = ctxproj-:Proj.coreFiles-:Defaults.projectDefault-:Files.modTime
        modtimetmpl = ctxproj-:Proj.coreFiles-:Defaults.htmlTemplateMain-:Files.modTime
    in listallfiles (cfgprocstatic-:ProjC.dirs) id >>= \allstaticfiles
    -> listallfiles (cfgprocpages-:ProjC.dirs) (max modtimetmpl) >>= \allpagesfiles_orig
    -> createindexhtmlifnocontentpages (allpagesfiles_orig~>length) >>= \defaultpage
    -> let
        filteroutfiles shouldforce fileinfos cfgproc =
            fileinfos >>| shouldbuildfile where
                skipall = ["*"]==cfgproc-:ProjC.skip
                forceall = ["*"]==cfgproc-:ProjC.force
                shouldbuildfile NoOutput =
                    pure False
                shouldbuildfile fileinfo =
                    let outfilepath = fileinfo-:outPathBuild
                        skipthis = (not skipall) && (matchesany $cfgproc-:ProjC.skip)
                        forcethis = (not forceall) && (matchesany $cfgproc-:ProjC.force)
                        matchesany = Files.simpleFileNameMatchAny $fileinfo-:relPath
                    in (forcethis || (forceall && not skipthis) || (shouldforce fileinfo))
                    |? pure True
                    |! (skipthis || (skipall && not forcethis))
                    |? pure False
                    |! let
                        ifexists False =
                            pure True
                        ifexists True =
                            System.Directory.getModificationTime outfilepath
                            >>= pure . ((fileinfo-:srcFile-:Files.modTime) >)
                    in System.Directory.doesFileExist outfilepath >>= ifexists
        outfileinfo contentdater relpather both@(relpath,file) =
            let (_,cdate) = Files.customDateFromFileName dtparser both   --  ignoring the renamed relpath as we already had to take it above (for bloks) when we had to ignore the cdate .. ugly this double call
                dtparser = ProjC.dtPageDateParse$ ctxproj-:Proj.setup-:Proj.cfg
                relpathnu = relpather relpath
                contentdate = contentdater (file-:Files.modTime , cdate)
            in (null relpathnu) |? NoOutput |! FileOutput {
                    relPath = relpathnu,
                    relPathSlashes = Files.pathSepSystemToSlash relpathnu,
                    blokName = Bloks.blokNameFromRelPath (ctxproj-:Proj.setup-:Proj.bloks) relpathnu file,
                    outPathBuild = ctxproj-:Proj.dirPathBuild </> relpathnu,
                    outPathDeploy = Util.ifIs (ctxproj-:Proj.dirPathDeploy) (</> relpathnu),
                    projPathCached = Util.ifIs (ctxproj-:Proj.dirPathCache) (</> relpathnu),
                    contentDate = contentdate,
                    srcFile = file
                }
        allpagesfiles_nodate = Data.List.sortBy blokpagesfirst (allpagesfiles_orig >~ renamerelpath) where
            blokpagesfirst (relpath1,file1) (relpath2,file2) =
                let cmp = compare b2 b1
                    b1 = Bloks.blokNameFromRelPath (projsetup-:Proj.bloks) relpath1 file1
                    b2 = Bloks.blokNameFromRelPath (projsetup-:Proj.bloks) relpath2 file2
                in if cmp /= EQ then cmp else
                    compare (file2-:Files.modTime) (file1-:Files.modTime)
            renamerelpath both@(_,file) =
                let (datelessrelpath , _modtime) = Files.customDateFromFileName (ProjC.dtPageDateParse projcfg) both
                    preferredrelpath = Bloks.preferredRelPath (projsetup-:Proj.bloks) (datelessrelpath , file)
                in (preferredrelpath , file)
        outfileinfobasic = outfileinfo fst id
        outfileinfopage = outfileinfo snd id
        outfileinfoatom filenamer = outfileinfo fst $filenamer.(Files.ensureFileExt True ".atom")
        allatoms = ((Posts.buildPlan modtimeproj (projcfg-:ProjC.relPathPostAtoms) (projsetup-:Proj.feeds)) ++ dynfeeds)
                    >~ (outfileinfoatom id)
        allstatics = allstaticfiles >~ outfileinfobasic
        allpages = (defaultpage==NoOutput) |? almostall |! defaultpage:almostall
                    where almostall = (allpagesfiles_nodate++dynpages) >~ outfileinfopage
        (dynpages,dynfeeds) = Bloks.buildPlan (modtimeproj,modtimetmpl) projcfg
                                                allpagesfiles_nodate $projsetup-:Proj.bloks
        sitemaprelpath = projcfg-:ProjC.relPathSiteMap
        sitemapbuildpath = ctxproj-:Proj.dirPathBuild </> sitemaprelpath
    in filteroutfiles (const False) allstatics cfgprocstatic >>= \outcopyfiles
    -> filteroutfiles (const False) allatoms cfgprocposts >>= \outatomfiles
    -> filteroutfiles (const False) (dynpages >~ outfileinfopage) cfgprocpages >>= \outdynpagefiles
    -> filteroutfiles (const True) allpages cfgprocpages >>= \sitemapfiles
    -> let shouldforce file =
            any dueto outatomfiles || any dueto outdynpagefiles where
                dueto = ((file-:blokName ==).blokName)
    in filteroutfiles shouldforce allpages cfgprocpages >>= \outpagefiles
    -> System.Directory.doesFileExist sitemapbuildpath >>= \sitemapexists
    -> let
        anyprocessing = outpagefiles~>length > 0
        sitemapbuild = FileOutput { relPath = sitemaprelpath, outPathBuild = sitemapbuildpath,
                                    outPathDeploy = Util.ifIs (ctxproj-:Proj.dirPathDeploy) (</> sitemaprelpath),
                                    projPathCached = "", relPathSlashes = "", blokName = "",
                                    contentDate = Util.dateTime0 , srcFile = Files.NoFile }
        sitemap = ( (has sitemaprelpath && anyprocessing) || (not sitemapexists) |? sitemapbuild |! NoOutput ,
                    sitemapfiles ~|relPath~.(Files.hasAnyFileExt $projcfg-:ProjC.htmlEquivExts) )
        feedjob outjob = Posts.BuildTask {
                                Posts.blokName = outjob-:blokName,
                                Posts.outPathBuild = outjob-:outPathBuild,
                                Posts.relPath = outjob-:relPath,
                                Posts.srcFile = outjob-:srcFile
                            }
    in pure BuildPlan {
                outAtoms = outatomfiles,
                outPages = outpagefiles,
                outStatics = outcopyfiles,
                numOutFilesTotal = (sitemap~>fst == NoOutput |? 0 |! 1) +
                                    outcopyfiles~>length + outpagefiles~>length + outatomfiles~>length,
                numDynPages = outdynpagefiles~>length,
                numSkippedStatic = allstatics~>length - outcopyfiles~>length,
                numSkippedPages = allpages~>length - outpagefiles~>length,
                numSkippedAtoms = allatoms~>length - outatomfiles~>length,
                anyProcessing = anyprocessing,
                siteMap = sitemap,
                allPagesFiles = allpagesfiles_nodate,
                feedJobs = outatomfiles >~ feedjob
            }
