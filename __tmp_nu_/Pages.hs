{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Pages where

import Base
import qualified Bloks
import qualified Build
import qualified Files
import qualified Html
import qualified Posts
import qualified Proj
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict
import qualified System.FilePath



processAll ctxmain ctxproj buildplan =
    let filenameexts = buildplan-:Build.outPages >~ filenameext
        filenameext = Build.srcFile ~. Files.path ~. System.FilePath.takeExtension
        ctxtmpl = ctxproj-:Proj.setup-:Proj.ctxTmpl
        cfgproj = ctxproj-:Proj.setup-:Proj.cfg
        ctxbuild = Posts.BuildContext Nothing
                                        (buildplan-:Build.allPagesFiles)
                                        (ctxproj-:Proj.setup-:Proj.bloks)
                                        (ctxproj-:Proj.setup-:Proj.posts)
                                        cfgproj

    in if null$ buildplan-:Build.outPages then return ([] , Data.Map.Strict.empty) else
    Tmpl.loadAll ctxmain ctxtmpl (ctxproj-:Proj.coreFiles)
                    filenameexts (cfgproj-:ProjC.htmlEquivExts) >>= \tmplfinder
    -> let foreach =
            processPage ctxmain ctxbuild ctxtmpl tmplfinder
        in (buildplan-:Build.outPages >>~ foreach)
        >>= \warnpages_and_srcpaths_and_pagerenders
        -> return ( warnpages_and_srcpaths_and_pagerenders>~fst ~> Util.unMaybes,
                    Data.Map.Strict.fromList (warnpages_and_srcpaths_and_pagerenders>~snd) )



processPage ctxmain ctxbuild ctxtmpl tmplfinder outjob =
    Files.writeTo dstfilepath (outjob-:Build.relPath) processcontent
    >>= \(outsrc , ctxpage , mismatches)
    -> Tmpl.warnIfTagMismatches ctxmain srcfilepath mismatches
    >> let i1 = Util.indexOfSub outsrc "|!}" ; i2 = Util.indexOfSub outsrc "{!|"
    in return ( (i2 < 0 || i1 < i2) |? Nothing |! (Just$ outjob-:Build.relPath),
                (outjob-:Build.srcFile-:Files.path , ctxpage))

    where
    dstfilepath = outjob-:Build.outPathBuild
    srcfilepath = outjob-:Build.srcFile-:Files.path

    loadsrccontent =
        let blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
        in if is blokindexname
            then return ((0,0) , "{X|_hax_blokindex: vars=[(\"bname\",\""++blokindexname++"\")], content=\"\" |}")
            else readFile srcfilepath >>= \rawsrc
                    -> return (Tmpl.tagMismatches rawsrc , rawsrc)

    processcontent =
        loadsrccontent >>= \(mismatches , pagesrc)
        -> let
            ctxpage = Tmpl.PageContext {
                            Tmpl.blokName = outjob-:Build.blokName,
                            Tmpl.pTagHandler = taghandler,
                            Tmpl.pVars = pagevars,
                            Tmpl.pDate = pagedate,
                            Tmpl.htmlInners = htmlinners,
                            Tmpl.htmlInner1st = htmlinner1st,
                            Tmpl.tmpl = tmpl,
                            Tmpl.cachedRenderSansTmpl = pageonlyproc,
                            Tmpl.allPagesFiles = ctxbuild-:Posts.allPagesFiles
                        }
            (pagevars , pagedate , pagesrcchunks) = pageVars (ctxbuild-:Posts.projCfg) pagesrc $outjob-:Build.contentDate
            taghandler = tagHandler (ctxbuild-:Posts.projCfg) ctxpage ctxtmpl outjob
            tmpl = tmplfinder$ System.FilePath.takeExtension dstfilepath
            pageonlyproc = Tmpl.processSrcFully ctxtmpl (Just ctxpage)
                            (null pagevars |? pagesrc |! (concat pagesrcchunks))
            outsrc = Tmpl.apply tmpl ctxpage pageonlyproc
            htmlinners tagname =
                Html.findInnerContentOfNoAttrTags tagname pageonlyproc
            htmlinner1st tagname defval =
                defval -|= (htmlinners tagname)@?0

        in return (outsrc , (outsrc , ctxpage , mismatches))



pageVars cfgproj pagesrc contentdate =
    (pagevars , pagedate , pagesrcchunks)
    where
    chunks = (Util.splitUp Util.trim ["{%P|"] "|%}" pagesrc)
    pagevars = chunks >~ foreach ~> Util.unMaybes
    pvardates = pagevars >~ maybedate ~> Util.unMaybes
    pagedate = contentdate -|= pvardates@?0
    pagesrcchunks = (chunks ~|null.snd) >~ fst -- ~|is

    foreach (pvarstr , "{%P|") =
        let nameandval = (Util.splitOn1st '=' pvarstr) ~> Util.bothTrim
        in Just nameandval
    foreach _ =
        Nothing

    maybedate (varname,varval) =
        let (dtpref , dtfname) = Util.bothTrim (Util.splitOn1st ':' varname)
        in if dtpref /= "date" then Nothing
            else ProjC.dtStr2Utc cfgproj dtfname varval



tagHandler cfgproj ctxpage ctxtmpl outjob ptagcontent
    | Util.startsWith ptagcontent "X|"
        = Just$ Tmpl.processXtagDelayed xtaghandler (drop 2 ptagcontent)
    | ptagcontent == "date"
        = fordate "" contentdate
    | dtfprefix == "date"
        = fordate dtfname contentdate
    | otherwise
        = case Data.List.lookup ptagcontent (ctxpage-:Tmpl.pVars) of
            Just val -> Just val
            Nothing -> for ptagcontent

    where
    xtaghandler = (ctxtmpl-:Tmpl.xTagHandler) (Just ctxpage)
    contentdate = ctxpage-:Tmpl.pDate
    (dtfprefix,dtfname) = Util.bothTrim (Util.splitOn1st ':' ptagcontent)
    fordate dtfn datetime =
        Just$ ProjC.dtUtc2Str cfgproj dtfn datetime
    for name =
        let (dtfp,dtfn) = Util.bothTrim (Util.splitOn1st ':' name)
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
                , "outBuild" =: outjob-:Build.outPathBuild
                , "outDeploy" =: outjob-:Build.outPathDeploy
                ]



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
            skip = case maybeblok of -- maybeblok~>((not . Bloks.inSitemap) =|- False) -- case maybeblok of
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
