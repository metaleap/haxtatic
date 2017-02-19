module Proj where

import Base
import qualified Lst
import qualified Str

import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Posts
import qualified ProjC
import qualified ProjT
import qualified Util
import qualified Tmpl
import qualified X

import qualified Data.Map.Strict
import qualified System.FilePath
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
        dirPathCache :: FilePath,
        setup :: Setup,
        coreFiles :: Defaults.Files
    }



data Setup
    = SetupFromProj {
        bloks :: Data.Map.Strict.Map String Bloks.Blok,
        feeds :: [String],
        posts :: [Posts.Item],
        cfg :: ProjC.Config,
        ctxTmpl :: Tmpl.CtxProc,
        tagMismatches :: (Int , Int),
        randSeed :: [Int]
    }



loadCtx ctxmain projname xregs defaultfiles =
    let loadedsetup = _loadSetup ctxmain ctxproj xregs defaultfiles
        dirpath = ctxmain-:Files.dirPath
        dirpathjoin = (dirpath </>)
        setupname = ctxmain-:Files.setupName
        ctxproj = ProjContext {
            projName = projname,
            setupName = setupname,
            domainName = (loadedsetup-:cfg-:ProjC.domainName) <?> projname,
            dirPath = dirpath,
            dirPathBuild = dirpathjoin$
                setupname ++ "-" ++ loadedsetup-:cfg-:ProjC.dirNameBuild,
            dirPathDeploy = let dd = loadedsetup-:cfg-:ProjC.dirNameDeploy
                            in (null dd) |? "" |! dirpathjoin $setupname++"-"++dd,
            dirPathCache = let dc = loadedsetup-:cfg-:ProjC.dirNameCache
                            in (null dc) |? "" |! dirpathjoin (dc </> setupname),
            setup = loadedsetup,
            coreFiles = defaultfiles
        }
    in pure ctxproj



_loadSetup ctxmain ctxproj xregs defaultfiles =
    SetupFromProj {
            bloks = blokspost, cfg = cfgpost, feeds = feedspost, posts = postspost,
            ctxTmpl = Tmpl.ProcessingContext {
                    Tmpl.bTagHandler =  Bloks.tagHandler blokspost,
                    Tmpl.cTagHandler = ProjC.tagHandler cfgmiscpost,
                    Tmpl.tTagHandler = ProjT.tagHandler ttagspost,
                    Tmpl.xTagHandler = X.tagHandler xtagspost,
                    Tmpl.processTags = cfgpost-:ProjC.tmplTags
                },
            tagMismatches = Tmpl.tagMismatches rawsrc,
            randSeed = (ctxmain-:Files.randSeed) ++ (rawsrc~>length) : (length $defaultfiles-:Defaults.htmlSnippets)
                            : (Util.dtInts $defaultfiles-:Defaults.projectDefault-:Files.modTime)
        }
    where
    setupprep = SetupFromProj {
            bloks = bloksprep, cfg = cfgprep, posts = postsprep, feeds = feedsprep,
            ctxTmpl = Tmpl.ProcessingContext {
                    Tmpl.bTagHandler =  Bloks.tagHandler bloksprep,
                    Tmpl.cTagHandler = ProjC.tagHandler cfgmiscprep,
                    Tmpl.tTagHandler = ProjT.tagHandler ttagsprep,
                    Tmpl.xTagHandler = X.tagHandler xtagsprep,
                    Tmpl.processTags = cfgprep-:ProjC.tmplTags
                },
            tagMismatches = (0,0), randSeed = []
        }
    bloksprep = Bloks.parseProjChunks cfgprep (pick prepchunkssplits 'B')
    blokspost = Bloks.parseProjChunks cfgpost (pick postchunkssplits 'B')
    (cfgprep,cfgmiscprep) = ProjC.parseProjChunks (pick prepchunkssplits 'C')
    (cfgpost,cfgmiscpost) = ProjC.parseProjChunks (pick postchunkssplits 'C')
    (feedsprep,postsprep) = Posts.parseProjChunks cfgprep (pick prepchunkssplits 'P')
    (feedspost,postspost) = Posts.parseProjChunks cfgpost (pick postchunkssplits 'P')
    ttagsprep = ProjT.parseProjChunks (pick prepchunkssplits 'T')
    ttagspost = ProjT.parseProjChunks (pick postchunkssplits 'T')
    xtagsprep = X.parseProjChunks ctxproj cfgprep xregs (pick prepchunkssplits 'X')
    xtagspost = X.parseProjChunks ctxproj cfgpost xregs (pick postchunkssplits 'X')
    pick chunkssplits prefix =
        (chunkssplits ~|(==prefix).fst) >~ snd

    prepchunkssplits = srcchunksprep >~ splitchunk
    postchunkssplits = srcchunkspost >~ splitchunk
    splitchunk (prefix,src) =
        (prefix , Lst.splitOn ':' src)
    rawsrc = _rawsrc ctxproj
    srcchunksprep = loadChunks rawsrc
    srcchunkspost = srcchunksprep >~ pretemplatechunk
    pretemplatechunk (prefix,src) =
        (prefix , Tmpl.processSrc (setupprep-:ctxTmpl) Nothing src)


loadChunks rawsrc =
    -- note to self, next time start from
    -- https://www.reddit.com/r/haskellquestions/comments/5r8wae/merging_list_elements_conditionally/dd6iv9c/
    paireds >~ rejointochunks
    where
    paireds = gatherothers beginners
    beginners = alllines ~| isbegin
    others = alllines ~|not.isbegin
    alllines = Util.indexed$ lines rawsrc

    isbegin (_ , '|':cfgprefix:'|':_) =
        any (cfgprefix==) ['B','C','P','T','X']
    isbegin _ =
        False

    gatherothers [] =
        []
    gatherothers ((thisindex,thisline):more) =
        let belongs i = (i > thisindex) && (null more || i < (fst$ more@!0))
        in (thisline , others ~|belongs.fst) : (null more |? [] |! gatherothers more)

    rejointochunks (beg,subs) =
        let sublines = subs >~ snd -- (Str.trim . snd) ~|is
            chunk = (Str.trim . unlines) ( (Str.trim beg):sublines )
        in (chunk@!1 , drop 3 chunk)



_rawsrc ctxproj =
    --  join primary project file with additionally-specified 'overwrites' ones:
    (ctxproj-:coreFiles-:Defaults.projectDefault-:Files.content) ++ "\n" ++
        (Lst.join '\n') (projcusts>~Files.content ++ projsnips>~foreach)
        where
        projcusts = ctxproj-:coreFiles-:Defaults.projectOverwrites
        projsnips = ctxproj-:coreFiles-:Defaults.htmlSnippets
        foreach snipfile =
            unlines (headerline:tabbedlines)
            where
            basename = (Util.dropLast (".haxsnip.html"~>length)) (System.FilePath.takeFileName $snipfile-:Files.path)
            headerline = "|X|hax.snippet:"++basename++":"
            tabbedlines = (lines $snipfile-:Files.content) >~ ("\t"++)
