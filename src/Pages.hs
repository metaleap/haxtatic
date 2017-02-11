module Pages where

import Hax.Base

import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
import qualified Html
import qualified Posts
import qualified Proj
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath



processAll ctxmain ctxproj buildplan =
    let filenameexts = buildplan-:Build.outPages >~ filenameext
        filenameext = Build.srcFile ~. Files.path ~. System.FilePath.takeExtension
        ctxtmpl = ctxproj-:Proj.setup-:Proj.ctxTmpl
        ctxbuildinitial = Posts.BuildContext (const Nothing) (buildplan-:Build.allPagesFiles)
                            (ctxproj-:Proj.setup-:Proj.bloks) (ctxproj-:Proj.setup-:Proj.posts) cfgproj
        cfgproj = ctxproj-:Proj.setup-:Proj.cfg

    in Tmpl.loadAll ctxmain ctxtmpl (ctxproj-:Proj.coreFiles)
                    filenameexts (cfgproj-:ProjC.htmlEquivExts) >>= \tmplfinder
    -> let
        process done [] =
            pure done
        process (prevwarns , prevhints , ctxbuildprev) (thisjob:morejobs) =
            procpage ctxbuildprev thisjob
            >>= \(maybewarning , maybehint , ctxbuildnext) ->
                let nextwarns = maybewarning~>((:prevwarns) =|- prevwarns)
                    nexthints = maybehint~>((:prevhints) =|- prevhints)
                in process (nextwarns , nexthints , ctxbuildnext) morejobs
        taghandler = tagHandler ctxmain cfgproj ctxtmpl
        procpage = processPage ctxmain ctxproj ctxtmpl tmplfinder taghandler

    in process ([] , [] , ctxbuildinitial) (buildplan-:Build.outPages)



processPage ctxmain ctxproj ctxtmpl tmplfinder thandler ctxbuild outjob =
    Files.writeTo dstfilepath (outjob-:Build.relPath) processcontent
    >>= \(outsrc , ctxpage , mismatches)
    -> Tmpl.warnIfTagMismatches ctxmain srcfilepath mismatches
    *> let
        outjobsrcfilepath = outjob-:Build.srcFile-:Files.path
        lookupcachedpagerender = ctxbuild-:Posts.lookupCachedPageRender
        cachelookup filepath
            |(filepath==outjobsrcfilepath)= Just ctxpage
            |(otherwise)= lookupcachedpagerender filepath
        i1 = Util.indexOfSub outsrc "|!}" ; i2 = Util.indexOfSub outsrc "{!|"
    in pure ( (i2 < 0 || i1 < i2) |? Nothing |! (Just$ outjob-:Build.relPath),
                (mismatches~>fst == mismatches~>snd) |? Nothing |! (Just$ outjob-:Build.relPath),
                ctxbuild { Posts.lookupCachedPageRender = cachelookup} )

    where
    dstfilepath = outjob-:Build.outPathBuild
    srcfilepath = outjob-:Build.srcFile-:Files.path
    cmpmodtime = max (outjob-:Build.srcFile-:Files.modTime) (ctxproj-:Proj.coreFiles-:Defaults.projectDefault-:Files.modTime)

    loadsrccontent =
        let blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
            tmpfilepath = Util.ifIs (outjob-:Build.projPathCached)
                            (++ (ProjC.dtPageDateFormat (ctxproj-:Proj.setup-:Proj.cfg) (outjob-:Build.contentDate)))
            tmpfilepathpvars = Util.ifIs tmpfilepath (++"pv")
            readtmp False =
                pure$ Files.FileFull tmpfilepath Util.dateTime0 ""
            readtmp True =
                System.Directory.getModificationTime tmpfilepath >>= \tmpmodtime
                -> if cmpmodtime > tmpmodtime then readtmp False else
                    readFile tmpfilepath >>= \tmpsrc
                    -> pure$ Files.FileFull tmpfilepath tmpmodtime tmpsrc
        in System.Directory.doesFileExist tmpfilepath >>= readtmp >>= \cachedfile
        -> System.Directory.doesFileExist tmpfilepathpvars >>= \istmpfilepvars
        -> if has blokindexname
            then pure ((0,0) , "{X|_hax_blokindex: vars=[(\"bname\",\""++blokindexname++"\")] , content=\"\" |}" , cachedfile)
            else readFile (if (istmpfilepvars && has (cachedfile-:Files.content)) then tmpfilepathpvars else srcfilepath) >>= \rawsrc
                    -> pure (Tmpl.tagMismatches rawsrc , rawsrc , cachedfile)

    processcontent =
        Data.Time.Clock.getCurrentTime >>= \nowtime
        -> loadsrccontent >>= \(mismatches , pagesrcraw, cachedfile)
        -> let
            pagesrctmp = cachedfile-:Files.content
            randseed' = (Util.dtInts nowtime)
                            ++ (Util.dtInts $outjob-:Build.srcFile-:Files.modTime)
                                ++ [ length $ctxbuild-:Posts.allPagesFiles , pagesrcraw~>length ]
            ctxpage htmlsrc ptaghandler = Tmpl.PageContext {
                                            Tmpl.blokName = outjob-:Build.blokName,
                                            Tmpl.pTagHandler = ptaghandler,
                                            Tmpl.pVars = pagevars,
                                            Tmpl.pDate = pagedate,
                                            Tmpl.htmlInners = htmlinners htmlsrc,
                                            Tmpl.htmlInner1st = htmlinner1st htmlsrc,
                                            Tmpl.tmpl = tmpl,
                                            Tmpl.cachedRenderSansTmpl = pagesrcproc,
                                            Tmpl.lookupCachedPageRender = ctxbuild-:Posts.lookupCachedPageRender,
                                            Tmpl.allPagesFiles = ctxbuild-:Posts.allPagesFiles,
                                            Tmpl.randSeed = randseed' >~ ((+) ((max 1 $pagesrcraw~>length) * randseed'@!1))
                                        }
            taghandler = thandler outjob
            ctxpageprep = ctxpage pagesrcraw (taghandler ctxpageprep)
            ctxpageproc = ctxpage pagesrcproc (taghandler ctxpageproc)
            (pagevars , pagedate , pagesrcchunks) = pageVars (ctxbuild-:Posts.projCfg) pagesrcraw $outjob-:Build.contentDate
            tmpl = tmplfinder$ System.FilePath.takeExtension dstfilepath
            pagesrcproc = if has pagesrctmp then pagesrctmp else
                            Tmpl.processSrc ctxtmpl (Just ctxpageprep)
                                (null pagevars |? pagesrcraw |! (concat pagesrcchunks))
            applied = Tmpl.apply tmpl ctxpageproc pagesrcproc
            --  annoyingly, thanks to nested-nestings there may easily *still* be fresh/pending haXtags,
            --  now that we did only-the-page-src AND the so-far unprocessed {P|'s in tmpl, so once more with feeling:
            outsrc = Tmpl.processSrc ctxtmpl (Just ctxpageproc) applied
            htmlinners htmlsrc tagname =
                Html.findInnerContentOfTags tagname htmlsrc
            htmlinner1st htmlsrc tagname defval =
                defval -|= (htmlinners htmlsrc tagname)@?0
            writetmp "" =
                let pvarstotmp = pagevars >>= foreach
                    foreach (pvname , pvval) =
                        "{%P|"++pvname++('=':(Tmpl.processSrc ctxtmpl (Just ctxpageproc) pvval))++"|%}"
                in Files.writeTo (cachedfile-:Files.path) "" (pure (pagesrcproc , ()))
                *> Files.writeTo (Util.ifIs (cachedfile-:Files.path) (++"pv")) "" (pure (pvarstotmp , ()))
            writetmp _ =
                pure ()
        in writetmp pagesrctmp
        *> pure (outsrc , (outsrc , ctxpageproc , mismatches))



pageVars cfgproj pagesrc contentdate =
    (pagevars , pagedate , pagesrcchunks)
    where
    chunks = (Util.splitUp Util.trim ["{%P|"] "|%}" pagesrc)
    pagevars = chunks>=~foreach
    pvardates = pagevars>=~maybedate
    pagedate = contentdate -|= pvardates@?0
    pagesrcchunks = (chunks ~|null.snd) >~ fst -- ~|is

    foreach (pvarstr , "{%P|") =
        let nameandval = (Util.splitOn1st_ '=' pvarstr) ~> Util.bothTrim
        in Just nameandval
    foreach _ =
        Nothing

    maybedate (varname,varval) =
        let (dtpref , dtfname) = Util.bothTrim (Util.splitOn1st_ ':' varname)
        in if dtpref /= "date" then Nothing
            else ProjC.dtStr2Utc cfgproj dtfname varval



tagHandler ctxmain cfgproj ctxtmpl outjob ctxpage ptagcontent
    | isxdelay ptagcontent
        = Just$ Tmpl.processXtagDelayed xtaghandler (drop 2 ptagcontent)
    | ptagcontent == "date"
        = fordate "" contentdate
    | split1st == "date"
        = fordate splitrest contentdate
    | has splitrest
        = ((Data.List.lookup split1st (ctxpage-:Tmpl.pVars)) >>= formatpvar) <|> (for ptagcontent)
    | otherwise
        = (Data.List.lookup ptagcontent (ctxpage-:Tmpl.pVars))
        <|> (for ptagcontent)
        <|> (ttags ('P':'|':ptagcontent))

    where
    ttags = ctxtmpl-:Tmpl.tTagHandler
    isxdelay ('X':'|':_) = True ; isxdelay _ = False
    xtaghandler = (ctxtmpl-:Tmpl.xTagHandler) (Just ctxpage)
    contentdate = ctxpage-:Tmpl.pDate
    (split1st , splitrest) = Util.bothTrim (Util.splitOn1st_ ':' ptagcontent)
    root2rel = Html.rootPathToRel (outjob-:Build.relPathSlashes)
    fordate dtfn datetime =
        Just$ ProjC.dtUtc2Str cfgproj dtfn datetime
    for ('/':path) =
        Just$ root2rel path
    for ('1':'s':'t':':':htmltagname) =
        Just$ (ctxpage-:Tmpl.htmlInner1st) (Util.trim htmltagname) ""
    for name =
        let (dtfp,dtfn) = Util.bothTrim (Util.splitOn1st_ ':' name)
        in if dtfp=="srcTime"
            then fordate dtfn (outjob-:Build.srcFile-:Files.modTime)
            else Data.List.lookup name pvals
    pvals = let reldir = Util.butNot "." "" (System.FilePath.takeDirectory$ outjob-:Build.relPath)
                reldir' = Util.butNot "." "" (System.FilePath.takeDirectory$ outjob-:Build.relPathSlashes)
            in  [ "title" =: (ctxpage-:Tmpl.htmlInner1st) "h1" ""
                , "fileBaseName" =: System.FilePath.takeBaseName (outjob-:Build.relPath)
                , "fileCoreName" =: drop (outjob-:Build.blokName~>length+1) (System.FilePath.takeBaseName $outjob-:Build.relPath)
                , "fileName" =: System.FilePath.takeFileName (outjob-:Build.relPath)
                , "fileUri" =: '/':(outjob-:Build.relPathSlashes)
                , "filePath" =: outjob-:Build.relPath
                , "dirName" =: Util.ifIs reldir System.FilePath.takeFileName
                , "dirUri" =: '/':(Util.ifIs reldir' (++"/"))
                , "dirPath" =: Util.ifIs reldir (++[System.FilePath.pathSeparator])
                , "srcPath" =: outjob-:Build.srcFile-:Files.path
                , "srcRelPath" =: drop (ctxmain-:Files.dirPath~>length+1) (outjob-:Build.srcFile-:Files.path)
                , "outBuild" =: outjob-:Build.outPathBuild
                , "outDeploy" =: outjob-:Build.outPathDeploy
                ]
    formatpvar pvarfmt =
        Util.formatWithList pvarfmt (Util.splitOn ':' splitrest)


writeSitemapXml ctxproj buildplan =
    let xmlsitemapfull xmlinneritems =
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
            \<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\">\n\
            \    "++(concat xmlinneritems)++"\n\
            \</urlset>"
        xmlsitemapitem domain relpath moddate priority =
            "<url>\n\
            \        <loc>http://"++domain++"/"++relpath++"</loc>\n\
            \        <lastmod>"++(ProjC.dtUtc2Str (ctxproj-:Proj.setup-:Proj.cfg) "" moddate)++"</lastmod>\n\
            \        <priority>"++(take 3 (show priority))++"</priority>\n\
            \    </url>"
        foreach pageinfo =
            skip |? "" |!
                xmlsitemapitem (ctxproj-:Proj.domainName) relpath (pageinfo-:Build.contentDate) priorel
            where
            maybeblok = Bloks.blokByName (ctxproj-:Proj.setup-:Proj.bloks) (pageinfo-:Build.blokName)
            skip = case maybeblok of
                    Just blok -> not$ blok-:Bloks.inSitemap
                    Nothing -> False
            relpath = pageinfo-:Build.relPathSlashes
            priorel = max 0.0 (priobase - priodown)

            priodown = (0.1::Double) * (fromIntegral$ (Util.count '/' relpath) + (Util.count '.' relpath) - (1::Int))
            priobase
                | relpath=="index.html" || relpath=="index.htm"
                = 1.0
                | Util.endsWith relpath "/index.html" || Util.endsWith relpath "/index.htm"
                = 0.88
                | otherwise
                = 0.66
        (outjob , pagefileinfos) = buildplan-:Build.siteMap
        xmlitems = (pagefileinfos >~foreach)
        xmloutput = pure (xmlsitemapfull xmlitems , ())
    in if outjob == Build.NoOutput
        then pure ()
        else Files.writeTo
                (outjob-:Build.outPathBuild)
                (outjob-:Build.relPath)
                xmloutput
            *> pure ()
