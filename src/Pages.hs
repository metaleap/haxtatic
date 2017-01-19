{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Pages where

import Base
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

import Control.Applicative ( (<|>) )
import qualified Data.List
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import qualified Text.Printf



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
        processpage done [] =
            return done
        processpage (prevwarns , prevhints , ctxbuildprev) (thisjob:morejobs) =
            processPage ctxmain ctxproj ctxbuildprev ctxtmpl tmplfinder thisjob
            >>= \(maybewarning , maybehint , ctxbuildnext) -> let
                nextwarns = maybewarning~>((:prevwarns) =|- prevwarns)
                nexthints = maybehint~>((:prevhints) =|- prevhints)
            in processpage (nextwarns , nexthints , ctxbuildnext) morejobs

    in processpage ([] , [] , ctxbuildinitial) (buildplan-:Build.outPages)



processPage ctxmain ctxproj ctxbuild ctxtmpl tmplfinder outjob =
    Files.writeTo dstfilepath (outjob-:Build.relPath) processcontent
    >>= \(outsrc , ctxpage , mismatches)
    -> Tmpl.warnIfTagMismatches ctxmain srcfilepath mismatches
    >> let
        outjobsrcfilepath = outjob-:Build.srcFile-:Files.path
        lookupcachedpagerender = ctxbuild-:Posts.lookupCachedPageRender
        cachelookup filepath
            |(filepath==outjobsrcfilepath)= Just ctxpage
            |(otherwise)= lookupcachedpagerender filepath
        i1 = Util.indexOfSub outsrc "|!}" ; i2 = Util.indexOfSub outsrc "{!|"
    in return ( (i2 < 0 || i1 < i2) |? Nothing |! (Just$ outjob-:Build.relPath),
                (mismatches~>fst == mismatches~>snd) |? Nothing |! (Just$ outjob-:Build.relPath),
                ctxbuild { Posts.lookupCachedPageRender = cachelookup} )

    where
    dstfilepath = outjob-:Build.outPathBuild
    srcfilepath = outjob-:Build.srcFile-:Files.path
    cmpmodtime = max (outjob-:Build.srcFile-:Files.modTime) (ctxproj-:Proj.coreFiles-:Defaults.projectDefault-:Files.modTime)

    loadsrccontent =
        let blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
            tmpfilepath = (outjob-:Build.projPathCached) ++
                            (ProjC.dtPageDateFormat (ctxproj-:Proj.setup-:Proj.cfg) (outjob-:Build.contentDate))
            readtmp False =
                return$ Files.FileFull tmpfilepath Util.dateTime0 ""
            readtmp True =
                System.Directory.getModificationTime tmpfilepath >>= \tmpmodtime
                -> if cmpmodtime > tmpmodtime then readtmp False else
                    readFile tmpfilepath >>= \tmpsrc
                    -> return$ Files.FileFull tmpfilepath tmpmodtime tmpsrc
        in System.Directory.doesFileExist tmpfilepath >>= readtmp >>= \cachedfile
        -> if has blokindexname
            then return ((0,0) , "{X|_hax_blokindex: vars=[(\"bname\",\""++blokindexname++"\")], content=\"\" |}" , cachedfile)
            else readFile srcfilepath >>= \rawsrc
                    -> return (Tmpl.tagMismatches rawsrc , rawsrc , cachedfile)

    processcontent =
        Data.Time.Clock.getCurrentTime >>= \nowtime
        -> loadsrccontent >>= \(mismatches , pagesrcraw, cachedfile)
        -> let
            pagesrctmp = cachedfile-:Files.content
            randseed' = (Util.dtInts nowtime)
                            ++ (Util.dtInts $outjob-:Build.srcFile-:Files.modTime)
                                ++ [ length $ctxbuild-:Posts.allPagesFiles , pagesrcraw~>length ]
            ctxpage htmlsrc thandler = Tmpl.PageContext {
                                            Tmpl.blokName = outjob-:Build.blokName,
                                            Tmpl.pTagHandler = thandler,
                                            Tmpl.pVars = pagevars,
                                            Tmpl.pDate = pagedate,
                                            Tmpl.htmlInners = htmlinners htmlsrc,
                                            Tmpl.htmlInner1st = htmlinner1st htmlsrc,
                                            Tmpl.tmpl = tmpl,
                                            Tmpl.cachedRenderSansTmpl = pagesrcproc,
                                            Tmpl.lookupCachedPageRender = ctxbuild-:Posts.lookupCachedPageRender,
                                            Tmpl.allPagesFiles = ctxbuild-:Posts.allPagesFiles,
                                            Tmpl.randSeed = randseed' >~ ((+) (pagesrcraw~>length * randseed'@!1))
                                        }
            ctxpageprep = ctxpage pagesrcraw (taghandler ctxpageprep)
            ctxpageproc = ctxpage pagesrcproc (taghandler ctxpageproc)
            (pagevars , pagedate , pagesrcchunks) = pageVars (ctxbuild-:Posts.projCfg) pagesrcraw $outjob-:Build.contentDate
            taghandler pagectx = tagHandler ctxmain (ctxbuild-:Posts.projCfg) pagectx ctxtmpl outjob
            tmpl = tmplfinder$ System.FilePath.takeExtension dstfilepath
            pagesrcproc = if has pagesrctmp then pagesrctmp else
                            Tmpl.processSrcFully ctxtmpl (Just ctxpageprep)
                                (null pagevars |? pagesrcraw |! (concat pagesrcchunks))
            applied = Tmpl.apply tmpl ctxpageproc pagesrcproc
            --  annoyingly, thanks to nested-nestings there may easily *still* be fresh/pending haXtags,
            --  now that we did only-the-page-src AND the so-far unprocessed {P|'s in tmpl, so once more with feeling:
            outsrc = Tmpl.processSrcFully ctxtmpl (Just ctxpageproc) applied
            htmlinners htmlsrc tagname =
                Html.findInnerContentOfTags tagname htmlsrc
            htmlinner1st htmlsrc tagname defval =
                defval -|= (htmlinners htmlsrc tagname)@?0
            writetmp "" = Files.writeTo (cachedfile-:Files.path) "" (return (pagesrcproc , ()))
            writetmp _ = return ()
        in writetmp pagesrctmp
        >> return (outsrc , (outsrc , ctxpageproc , mismatches))



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



tagHandler ctxmain cfgproj ctxpage ctxtmpl outjob ptagcontent
    | Util.startsWith ptagcontent "X|"
        = Just$ Tmpl.processXtagDelayed xtaghandler (drop 2 ptagcontent)
    | ptagcontent == "date"
        = fordate "" contentdate
    | split1st == "date"
        = fordate splitrest contentdate
    | has splitrest
        = (Data.List.lookup split1st (ctxpage-:Tmpl.pVars)) ~> (formatpvar =|- for ptagcontent)
    | otherwise
        = (Data.List.lookup ptagcontent (ctxpage-:Tmpl.pVars)) <|> (for ptagcontent)

    where
    xtaghandler = (ctxtmpl-:Tmpl.xTagHandler) (Just ctxpage)
    contentdate = ctxpage-:Tmpl.pDate
    (split1st , splitrest) = Util.bothTrim (Util.splitOn1st_ ':' ptagcontent)
    fordate dtfn datetime =
        Just$ ProjC.dtUtc2Str cfgproj dtfn datetime
    for ('/':path) =
        let newrelpath = take (Util.count '/' (outjob-:Build.relPathSlashes)) infinity
            infinity = iterate (++ "../") "../"
        in Just$ ( null newrelpath |? path |! ((last newrelpath) ++ path) )
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
                , "fileBaseName" =: (System.FilePath.takeBaseName$ outjob-:Build.relPath)
                , "fileName" =: (System.FilePath.takeFileName$ outjob-:Build.relPath)
                , "fileUri" =: '/':(outjob-:Build.relPathSlashes)
                , "filePath" =: outjob-:Build.relPath
                , "dirName" =: Util.ifIs reldir System.FilePath.takeFileName
                , "dirUri" =: '/':(Util.ifIs reldir' (++"/"))
                , "dirPath" =: Util.ifIs reldir (++[System.FilePath.pathSeparator])
                , "srcPath" =: outjob-:Build.srcFile-:Files.path
                , "srcRelPath" =: (drop (ctxmain-:Files.dirPath~>length+1) (outjob-:Build.srcFile-:Files.path))
                , "outBuild" =: outjob-:Build.outPathBuild
                , "outDeploy" =: outjob-:Build.outPathDeploy
                ]
    formatpvar pvarfmt =
        -- OUCH some ugly hackery! will do for a time
        let num = count 0 pvarfmt
            count c [] = c
            count c ('%':'s':more) = count (c + 1) more
            count c ('%':'v':more) = count (c + 1) more
            count c (_:more) = count c more
            args = (take num $ cycle (Util.splitOn ':' splitrest))
            arg1 func [ _1 ] = func _1 ; arg1 _ _ = undefined
            arg2 func [ _1 , _2 ] = func _1 _2 ; arg2 _ _ = undefined
            arg3 func [ _1 , _2 , _3 ] = func _1 _2 _3 ; arg3 _ _ = undefined
            arg4 func [ _1 , _2 , _3 , _4 ] = func _1 _2 _3 _4 ; arg4 _ _ = undefined
            arg5 func [ _1 , _2 , _3 , _4 , _5 ] = func _1 _2 _3 _4 _5 ; arg5 _ _ = undefined
            arg6 func [ _1 , _2 , _3 , _4 , _5 , _6 ] = func _1 _2 _3 _4 _5 _6 ; arg6 _ _ = undefined
        in case num of
            0 -> for ptagcontent
            1 -> Just$ arg1 (Text.Printf.printf pvarfmt) args
            2 -> Just$ arg2 (Text.Printf.printf pvarfmt) args
            3 -> Just$ arg3 (Text.Printf.printf pvarfmt) args
            4 -> Just$ arg4 (Text.Printf.printf pvarfmt) args
            5 -> Just$ arg5 (Text.Printf.printf pvarfmt) args
            6 -> Just$ arg6 (Text.Printf.printf pvarfmt) args
            _ -> for ptagcontent



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

            priodown = 0.1 * (fromIntegral$ (Util.count '/' relpath) + (Util.count '.' relpath) - 1) :: Float
            priobase
                | relpath=="index.html" || relpath=="index.htm"
                = 1.0
                | Util.endsWith relpath "/index.html" || Util.endsWith relpath "/index.htm"
                = 0.88
                | otherwise
                = 0.66
        (outjob , pagefileinfos) = buildplan-:Build.siteMap
        xmlitems = (pagefileinfos >~foreach)
        xmloutput = return (xmlsitemapfull xmlitems , ())
    in if outjob == Build.NoOutput
        then return ()
        else Files.writeTo
                (outjob-:Build.outPathBuild)
                (outjob-:Build.relPath)
                xmloutput
            >> return ()
