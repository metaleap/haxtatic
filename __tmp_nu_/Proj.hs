{-# OPTIONS_GHC -Wall #-}
module Proj where

import Base
import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Posts
import qualified ProjC
import qualified ProjT
import qualified Tmpl
import qualified Util
import qualified X

import qualified Tmpl

import qualified Data.Map.Strict
import System.FilePath ( (</>) )



--  project context
data Ctx
    = ProjContext {
        projName :: String,
        setupName :: String,
        domainName :: String,
        dirPath :: FilePath,
        dirPathBuild :: FilePath,
        dirPathDeploy :: FilePath,
        setup :: Setup,
        coreFiles :: Defaults.Files
    }


data Setup
    = SetupFromProj {
        tmpDbgSrc :: [(Char,String)],
        bloks :: Data.Map.Strict.Map String Bloks.Blok,
        feeds :: [String],
        posts :: [Posts.Post],
        cfg :: ProjC.Config,
        tmpl :: Tmpl.CtxProc,
        tagMismatches :: (Int , Int)
    }



loadCtx ctxmain projname xregs defaultfiles =
    let loadedsetup = _loadSetup ctxproj xregs
        dirpath = ctxmain.:Files.dirPath
        dirpathjoin = (dirpath </>)
        setupname = ctxmain.:Files.setupName
        ctxproj = ProjContext {
            projName = projname,
            setupName = setupname,
            domainName = Util.ifNo (loadedsetup.:cfg.:ProjC.domainName) projname,
            dirPath = dirpath,
            dirPathBuild = dirpathjoin$
                setupname ++ "-" ++ loadedsetup.:cfg.:ProjC.dirNameBuild,
            dirPathDeploy = let dd = loadedsetup.:cfg.:ProjC.dirNameDeploy
                            in (null dd) |? "" |! dirpathjoin $setupname++"-"++dd,
            setup = loadedsetup,
            coreFiles = defaultfiles
        }
    in return ctxproj



_loadSetup ctxproj xregs =
    SetupFromProj { tmpDbgSrc = srcchunkspost, bloks = blokspost,
                    cfg = cfgpost, posts = postspost, feeds = feedspost,
                    tmpl = Tmpl.ProcessingContext {
                            Tmpl.bTagHandler =  Bloks.tagHandler blokspost,
                            Tmpl.cTagHandler = ProjC.tagHandler cfgmiscpost,
                            Tmpl.tTagHandler = ProjT.tagHandler ttagspost,
                            Tmpl.xTagHandler = X.tagHandler xtagspost,
                            Tmpl.processTags = cfgpost.:ProjC.tmplTags
                        },
                    tagMismatches = Tmpl.tagMismatches rawsrc
                    }
    where
    setupprep = SetupFromProj { tmpDbgSrc = srcchunksprep, bloks = bloksprep,
                                cfg = cfgprep, posts = postsprep, feeds = feedsprep,
                                tmpl = Tmpl.ProcessingContext {
                                        Tmpl.bTagHandler =  Bloks.tagHandler bloksprep,
                                        Tmpl.cTagHandler = ProjC.tagHandler cfgmiscprep,
                                        Tmpl.tTagHandler = ProjT.tagHandler ttagsprep,
                                        Tmpl.xTagHandler = X.tagHandler xtagsprep,
                                        Tmpl.processTags = cfgprep.:ProjC.tmplTags
                                    },
                                tagMismatches = (0,0)
                                }
    bloksprep = Bloks.parseProjChunks (pick prepchunkssplits 'B')
    blokspost = Bloks.parseProjChunks (pick postchunkssplits 'B')
    (cfgprep,cfgmiscprep) = ProjC.parseProjChunks (pick prepchunkssplits 'C')
    (cfgpost,cfgmiscpost) = ProjC.parseProjChunks (pick postchunkssplits 'C')
    (feedsprep,postsprep) = Posts.parseProjChunks (pick prepchunkssplits 'P')
    (feedspost,postspost) = Posts.parseProjChunks (pick postchunkssplits 'P')
    ttagsprep = ProjT.parseProjChunks False (pick prepchunkssplits 'T')
    ttagspost = ProjT.parseProjChunks True (pick postchunkssplits 'T')
    xtagsprep = X.parseProjChunks xregs (pick prepchunkssplits 'X')
    xtagspost = X.parseProjChunks xregs (pick postchunkssplits 'X')
    pick chunkssplits prefix =
        (chunkssplits ~|(==prefix).fst) >~ snd

    prepchunkssplits = srcchunksprep >~ splitchunk
    postchunkssplits = srcchunkspost >~ splitchunk
    splitchunk (prefix,src) =
        (prefix , Util.splitOn ':' src)
    rawsrc = _rawsrc ctxproj
    srcchunksprep = loadChunks rawsrc
    srcchunkspost = srcchunksprep >~ pretemplatechunk
    pretemplatechunk (prefix,src) =
        (prefix , Tmpl.processSrcFully (setupprep.:tmpl) Nothing src)


loadChunks rawsrc =
    paireds >~ rejointochunks
    where
    paireds = gatherothers beginners
    beginners = alllines ~| isbegin
    others = alllines ~|not.isbegin
    alllines = Util.indexed$ lines rawsrc

    isbegin (index,'|':cfgprefix:'|':_) =
        any (cfgprefix==) "BCPTX"
    isbegin _ =
        False

    gatherothers [] =
        []
    gatherothers ((thisindex,thisline):more) =
        let belongs i = (i > thisindex) && (null more || i < (fst$ more#0))
        in (thisline , others ~|belongs.fst) : (null more |? [] |! gatherothers more)

    rejointochunks (beg,subs) =
        let sublines = subs >~ snd -- (Util.trim . snd) ~|is
            chunk = (Util.trim . unlines) ( (Util.trim beg):sublines )
        in (chunk#1 , drop 3 chunk)



_rawsrc ctxproj =
    --  join primary project file with additionally-specified 'overwrites' ones:
    (ctxproj.:coreFiles.:Defaults.projectDefault.:Files.content) ++
        let projcusts = ctxproj.:coreFiles.:Defaults.projectOverwrites in
            concat$ projcusts>~Files.content
