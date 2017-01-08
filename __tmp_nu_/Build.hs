{-# OPTIONS_GHC -Wall #-}
module Build where

import Base
import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Proj
import qualified ProjC
import qualified Util

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.IO
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
        siteMap :: (Task , [Task])
    }


data Task
    = NoOutput
    | FileOutput {
        relPath :: FilePath,
        relPathSlashes :: FilePath,
        blokName :: String,
        outPathBuild :: FilePath,
        outPathDeploy :: FilePath,
        contentDate :: Data.Time.Clock.UTCTime,
        srcFile :: Files.File
    }
    deriving (Eq)



copyAllOutputsToDeploy buildplan =
    let foreach NoOutput =
            return ()
        foreach builtfile =
            let srcfilepath = builtfile.:outPathBuild
                ifexists True =
                    Files.copyTo srcfilepath [builtfile.:outPathDeploy]
                ifexists False =
                    putStrLn ("\t-!\tMissing: `" ++srcfilepath++ "`")
            in System.Directory.doesFileExist srcfilepath >>= ifexists
    in (buildplan.:outStatics) >>~ foreach
    >> (buildplan.:outPages) >>~ foreach
    >> (buildplan.:outAtoms) >>~ foreach
    >> (buildplan.:siteMap~>fst) ~> foreach
    >> return ()



copyStaticFiles buildplan =
    (buildplan.:outStatics) >>~ foreach where
        foreach file =
            Files.copyTo (file.:srcFile.:Files.path) [file.:outPathBuild]



_createIndexHtmlIfNoContentPages ctxmain ctxproj numpagesrcfiles =
    if numpagesrcfiles > 0 then return NoOutput
    else let
        sitename = ctxproj.:Proj.projName
        dirpagesrel = (ctxproj.:Proj.setup.:Proj.cfg.:ProjC.processingOfPages.:ProjC.dirs)#0
        dirbuild = ctxproj.:Proj.dirPathBuild
        htmltemplatemain = ctxproj.:Proj.coreFiles.:Defaults.htmlTemplateMain
    in putStrLn ("\t->\tNo content-source files whatsoever.. making one for you:")
    >> Defaults.writeDefaultIndexHtml
        ctxmain sitename dirpagesrel dirbuild htmltemplatemain
    >>= \(srcfile , relpath , outjobpath)
    -> return FileOutput {
                    relPath = relpath,
                    relPathSlashes = Files.pathSepSystemToSlash relpath,
                    blokName = "",
                    outPathBuild = outjobpath,
                    outPathDeploy = Util.ifIs (ctxproj.:Proj.dirPathDeploy) (</> relpath),
                    contentDate = srcfile.:Files.modTime,
                    srcFile = srcfile
                }



plan ctxmain ctxproj =
    let projsetup = ctxproj.:Proj.setup
        projcfg = projsetup.:Proj.cfg
        cfgprocstatic = projcfg.:ProjC.processingOfFiles
        cfgprocpages = projcfg.:ProjC.processingOfPages
        cfgprocposts = projcfg.:ProjC.processingOfPosts
        listallfiles = Files.listAllFiles $ctxproj.:Proj.dirPath
        modtimeproj = ctxproj.:Proj.coreFiles.:Defaults.projectDefault.:Files.modTime
        modtimetmplmain = ctxproj.:Proj.coreFiles.:Defaults.htmlTemplateMain.:Files.modTime
        modtimetmplblok = ctxproj.:Proj.coreFiles.:Defaults.htmlTemplateBlok.:Files.modTime
    in listallfiles (cfgprocstatic.:ProjC.dirs) id >>= \allstaticfiles
    -> listallfiles (cfgprocposts.:ProjC.dirs) (max modtimeproj) >>= \allpostsfiles
    -> listallfiles (cfgprocpages.:ProjC.dirs) (max modtimetmplmain) >>= \allpagesfiles_orig
    -> _createIndexHtmlIfNoContentPages ctxmain ctxproj (allpagesfiles_orig~>length) >>= \defaultpage
    -> let
        allpagesfiles_nodate = allpagesfiles_orig >~ renamerelpath where
            renamerelpath both@(_,file) =
                (fst$ Files.customDateFromFileName (ProjC.dtPageDateParse projcfg) both , file)
        outfileinfobasic = _outFileInfo ctxproj fst id
        outfileinfopage = _outFileInfo ctxproj snd id
        outfileinfoatom filenamer = _outFileInfo ctxproj fst $filenamer.(Files.ensureFileExt True ".atom")
        outfileinfopost |(rppostatoms==Defaults.dir_PostAtoms_None)= outfileinfoatom (const "")
                        |(null rppostatoms)= outfileinfoatom id
                        |(otherwise)= outfileinfoatom (rppostatoms </>)
                        where rppostatoms = projcfg.:ProjC.relPathPostAtoms
        allatoms = (allpostsfiles>~outfileinfopost) ++ (dynatoms>~(outfileinfoatom id))
        allstatics = allstaticfiles >~ outfileinfobasic
        allpages = (defaultpage==NoOutput) |? almostall |! defaultpage:almostall
                    where almostall = (allpagesfiles_nodate++dynpages) >~ outfileinfopage
        (dynpages,dynatoms) = Bloks.buildPlan (modtimeproj,modtimetmplblok) projcfg
                                                allpagesfiles_nodate $projsetup.:Proj.bloks
        sitemaprelpath = projcfg.:ProjC.relPathSiteMap
        sitemapbuildpath = ctxproj.:Proj.dirPathBuild </> sitemaprelpath
    in _filterOutFiles False allstatics cfgprocstatic >>= \outcopyfiles
    -> _filterOutFiles False allpages cfgprocpages >>= \outpagefiles
    -> _filterOutFiles False (dynpages >~ outfileinfopage) cfgprocpages >>= \abitwasteful_justtogetanum
    -> _filterOutFiles False allatoms cfgprocposts >>= \outatomfiles
    -> _filterOutFiles True allpages cfgprocpages >>= \sitemapfiles
    -> System.Directory.doesFileExist sitemapbuildpath >>= \sitemapexists
    -> let
        anyprocessing = outpagefiles~>length > 0
        sitemapbuild = FileOutput { relPath = sitemaprelpath, outPathBuild = sitemapbuildpath,
                                    outPathDeploy = Util.ifIs (ctxproj.:Proj.dirPathDeploy) (</> sitemaprelpath) }
        sitemap = ( (is sitemaprelpath && anyprocessing) || (not sitemapexists) |? sitemapbuild |! NoOutput ,
                    sitemapfiles ~|relPath~.(Files.hasAnyFileExt $projcfg.:ProjC.htmlEquivExts) )
    in return BuildPlan {
                outAtoms = outatomfiles,
                outPages = outpagefiles,
                outStatics = outcopyfiles,
                numOutFilesTotal = (sitemap~>fst == NoOutput |? 0 |! 1) +
                                    outcopyfiles~>length + outpagefiles~>length + outatomfiles~>length,
                numDynPages = abitwasteful_justtogetanum~>length,
                numSkippedStatic = allstatics~>length - outcopyfiles~>length,
                numSkippedPages = allpages~>length - outpagefiles~>length,
                numSkippedAtoms = allatoms~>length - outatomfiles~>length,
                anyProcessing = anyprocessing,
                siteMap = sitemap
            }



_outFileInfo ctxproj contentdater relpather both@(relpath,file) =
    let (_,cdate) = Files.customDateFromFileName dtparser both   --  ignoring the renamed relpath as we already had to take it above (for bloks) when we had to ignore the cdate .. ugly this double call
        dtparser = ProjC.dtPageDateParse$ ctxproj.:Proj.setup.:Proj.cfg
        relpathnu = relpather relpath
        contentdate = contentdater (file.:Files.modTime , cdate)
    in (null relpathnu) |? NoOutput |! FileOutput {
            relPath = relpathnu,
            relPathSlashes = Files.pathSepSystemToSlash relpathnu,
            blokName = Bloks.blokNameFromRelPath (ctxproj.:Proj.setup.:Proj.bloks) relpathnu file,
            outPathBuild = ctxproj.:Proj.dirPathBuild </> relpathnu,
            outPathDeploy = Util.ifIs (ctxproj.:Proj.dirPathDeploy) (</> relpathnu),
            contentDate = contentdate,
            srcFile = file
        }



_filterOutFiles forceforceall fileinfos cfgproc =
    fileinfos >>| shouldbuildfile where
        skipall = ["*"]==cfgproc.:ProjC.skip
        forceall = ["*"]==cfgproc.:ProjC.force
        shouldbuildfile NoOutput =
            return False
        shouldbuildfile fileinfo =
            let skipthis = (not skipall) && (matchesany $cfgproc.:ProjC.skip)
                forcethis = (not forceall) && (matchesany $cfgproc.:ProjC.force)
                matchesany = Files.simpleFileNameMatchAny $fileinfo.:relPath
            in forcethis || (forceall && not skipthis) || forceforceall
            |? return True
            |! skipthis || (skipall && not forcethis)
            |? return False
            |! let
                outfilepath = fileinfo.:outPathBuild
                ifexists False =
                    return True
                ifexists True =
                    System.Directory.getModificationTime outfilepath
                    >>= return.((fileinfo.:srcFile.:Files.modTime) >)
            in System.Directory.doesFileExist outfilepath >>= ifexists
