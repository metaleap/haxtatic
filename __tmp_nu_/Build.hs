{-# OPTIONS_GHC -Wall #-}

module Build where

import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Proj
import qualified ProjCfg
import qualified Util
import Util ( (~:) , (>~) , (>>~) , (>>|) , (#) )

import qualified Data.Time.Clock
import qualified System.Directory
import System.FilePath ( (</>) )



data Plan = BuildPlan {
    outAtoms :: [Task],
    outPages :: [Task],
    outStatics :: [Task],
    numOutFilesTotal :: Int,
    numSkippedStatic :: Int,
    numSkippedPages :: Int,
    numSkippedAtoms :: Int
}


data Task = NoOutput | FileOutput {
    relPath :: FilePath,
    outPathBuild :: FilePath,
    outPathDeploy :: FilePath,
    contentDate :: Data.Time.Clock.UTCTime,
    srcFile :: Files.File
} deriving (Eq)



copyAllOutputsToDeploy buildplan =
    let foreach builtfile =
            let srcfilepath = builtfile~:outPathBuild
                ifexists True =
                    Files.copyTo srcfilepath [builtfile~:outPathDeploy]
                ifexists False =
                    putStrLn ("\t!?\tMissing: `"++srcfilepath++"`")
            in System.Directory.doesFileExist srcfilepath >>= ifexists
    in (buildplan~:outStatics) >>~ foreach
    >> (buildplan~:outPages) >>~ foreach
    >> (buildplan~:outAtoms) >>~ foreach
    >> return ()



copyStaticFiles buildplan =
    (buildplan~:outStatics) >>~ foreach where
        foreach file =
            Files.copyTo (file~:srcFile~:Files.path) [file~:outPathBuild]



_createIndexHtmlIfNoContentPages ctxmain ctxproj numpagesrcfiles =
    if numpagesrcfiles > 0
        then return NoOutput
        else let
            sitename = ctxproj~:Proj.projName
            dirpagesrel = (ctxproj~:Proj.setup~:Proj.cfg~:ProjCfg.processPages~:ProjCfg.dirs)#0
            dirbuild = ctxproj~:Proj.dirPathBuild
            htmltemplatemain = ctxproj~:Proj.coreFiles~:Defaults.htmlTemplateMain
        in putStrLn ("\t->\tNo content-source files whatsoever.. making one for you:")
        >> Defaults.writeDefaultIndexHtml
            ctxmain sitename dirpagesrel dirbuild htmltemplatemain
        >>= \ (outfile , outfilerel , pathfinal)
        -> return FileOutput {
                        relPath = outfilerel,
                        outPathBuild = pathfinal,
                        outPathDeploy = Util.unlessNullOp (ctxproj~:Proj.dirPathDeploy) (</> outfilerel),
                        contentDate = outfile~:Files.modTime,
                        srcFile = outfile
                    }



plan ctxmain ctxproj =
    let projsetup = ctxproj~:Proj.setup
        projcfg = projsetup~:Proj.cfg
        cfgprocstatic = projcfg~:ProjCfg.processStatic
        cfgprocpages = projcfg~:ProjCfg.processPages
        cfgprocposts = projcfg~:ProjCfg.processPosts
        listallfiles = Files.listAllFiles $ctxproj~:Proj.dirPath
        modtimeproj = ctxproj~:Proj.coreFiles~:Defaults.projectDefault~:Files.modTime
        modtimetmplmain = ctxproj~:Proj.coreFiles~:Defaults.htmlTemplateMain~:Files.modTime
        modtimetmplblok = ctxproj~:Proj.coreFiles~:Defaults.htmlTemplateBlok~:Files.modTime
    in listallfiles (cfgprocstatic~:ProjCfg.dirs) id >>= \allstaticfiles
    -> listallfiles (cfgprocposts~:ProjCfg.dirs) (max modtimeproj) >>= \allpostsfiles
    -> listallfiles (cfgprocpages~:ProjCfg.dirs) (max modtimetmplmain) >>= \allpagesfiles_orig
    -> _createIndexHtmlIfNoContentPages ctxmain ctxproj (allpagesfiles_orig~:length) >>= \ defaultpage
    -> let
        allpagesfiles_nodate = allpagesfiles_orig >~ renamerelpath where
            renamerelpath tup@(_,file) =
                (fst$ Files.customDateFromFileName (_dateparser projcfg) tup , file)
        outfileinfobasic = _outFileInfo ctxproj fst id
        outfileinfopage = _outFileInfo ctxproj snd id
        outfileinfoatom func = _outFileInfo ctxproj fst $func.(Files.ensureFileExt True ".atom")
        outfileinfopost = outfileinfoatom func where
            func|(null relpathpostatoms)= id                                -- no custom dir for posts-derived atoms set up
                |(relpathpostatoms==Defaults.dir_PostAtoms_None)= const ""  -- dont generate atoms -> force "" to discard in _filterOutFiles
                |(otherwise)= (relpathpostatoms </>)                        -- prepend user-specified rel dir to atom out-file name
            relpathpostatoms = projcfg~:ProjCfg.relPathPostAtoms
        allatoms = (allpostsfiles>~outfileinfopost) ++ (dynatoms>~(outfileinfoatom id))
        allstatics = allstaticfiles >~ outfileinfobasic
        allpages = let almostall = (dynpages++allpagesfiles_nodate) >~ outfileinfopage
                    in if defaultpage==NoOutput then (almostall) else
                        defaultpage:almostall
        (dynpages,dynatoms) = Bloks.buildPlan (modtimeproj,modtimetmplblok) allpagesfiles_nodate $projsetup~:Proj.bloks
    in _filterOutFiles allstatics cfgprocstatic >>= \outcopyfiles
    -> _filterOutFiles allpages cfgprocpages >>= \outpagefiles
    -> _filterOutFiles allatoms cfgprocposts >>= \outatomfiles
    -> let buildplan = BuildPlan {
                outAtoms = outatomfiles,
                outPages = outpagefiles,
                outStatics = outcopyfiles,
                numOutFilesTotal = outcopyfiles~:length + outpagefiles~:length + outatomfiles~:length,
                numSkippedStatic = allstatics~:length - outcopyfiles~:length,
                numSkippedPages = allpages~:length - outpagefiles~:length,
                numSkippedAtoms = allatoms~:length - outatomfiles~:length
            }
    in return buildplan


_dateparser projcfg = Proj.dtStr2Utc projcfg "hax_pagedate"


_outFileInfo ctxproj contentdater relpather tup@(relpath,file) =
    let (_,cdate) = Files.customDateFromFileName dtparser tup   --  ignoring the renamed relpath as we already had to take it above (for bloks) when we had to ignore the cdate .. ugly this double call
        dtparser = _dateparser $ctxproj~:Proj.setup~:Proj.cfg
        relpathnu = relpather relpath
        contentdate = contentdater (file~:Files.modTime , cdate)
    in if null relpathnu
        then NoOutput
        else FileOutput {
            relPath = relpathnu,
            outPathBuild = ctxproj~:Proj.dirPathBuild </> relpathnu,
            outPathDeploy = Util.unlessNullOp (ctxproj~:Proj.dirPathDeploy) (</> relpathnu),
            contentDate = contentdate,
            srcFile = file
        }



_filterOutFiles fileinfos cfgproc =
    fileinfos >>| shouldbuildfile where
        skipall = ["*"]== cfgproc~:ProjCfg.skip
        forceall = ["*"]== cfgproc~:ProjCfg.force
        shouldbuildfile NoOutput =
            return False
        shouldbuildfile fileinfo =
            let skipthis = (not skipall) && (matchesany $cfgproc~:ProjCfg.skip)
                forcethis = (not forceall) && (matchesany $cfgproc~:ProjCfg.force)
                matchesany = Files.simpleFileNameMatchAny $fileinfo~:relPath
                outfilepath = fileinfo~:outPathBuild
            in if (forceall && not skipthis) || forcethis
                then return True else
                if (skipall && not forcethis) || skipthis
                    then return False else
                    let ifexists False =
                            return True
                        ifexists True =
                            System.Directory.getModificationTime outfilepath >>=
                                return.((fileinfo~:srcFile~:Files.modTime)>)
                    in System.Directory.doesFileExist outfilepath >>= ifexists
