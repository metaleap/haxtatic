{-# OPTIONS_GHC -Wall #-}
module Pages where

import Base
import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
import qualified Html
import qualified Proj
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO



processAll ctxmain ctxproj buildplan =
    let filenameexts = buildplan.:Build.outPages >~ filenameext
        filenameext = Build.srcFile ~. Files.path ~. System.FilePath.takeExtension
        ctxtmpl = ctxproj.:Proj.setup.:Proj.tmpl
        cfgproj = ctxproj.:Proj.setup.:Proj.cfg

    in Tmpl.loadAll
                ctxmain
                ctxtmpl
                (ctxproj.:Proj.coreFiles)
                filenameexts
                (cfgproj.:ProjC.htmlEquivExts)
        >>= \tmplfinder
    -> let foreach buildtask =
            processPage ctxmain cfgproj ctxtmpl tmplfinder buildtask
        in buildplan.:Build.outPages >>~ foreach
        >> return ()



processPage ctxmain cfgproj ctxtmpl tmplfinder outjob =
    Files.writeTo dstfilepath (outjob.:Build.relPath) processcontent
    >>= Tmpl.warnIfTagMismatches ctxmain srcfilepath

    where
    dstfilepath = outjob.:Build.outPathBuild
    srcfilepath = outjob.:Build.srcFile.:Files.path

    loadsrccontent =
        let blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
            blokindextmpl = tmplfinder Defaults.blokIndexPrefix
        in if is blokindexname
            then return ((0,0) , blokindextmpl.:Tmpl.srcFile.:Files.content)
            else readFile srcfilepath >>= \rawsrc
                    -> return (Tmpl.tagMismatches rawsrc , rawsrc)

    processcontent =
        loadsrccontent >>= \(mismatches , pagesrc)
        -> let
            ctxpage = Tmpl.PageContext {
                            Tmpl.blokName = outjob.:Build.blokName,
                            Tmpl.pTagHandler = taghandler,
                            Tmpl.pVars = pagevars,
                            Tmpl.pDate = pagedate,
                            Tmpl.htmlInners = htmlinners,
                            Tmpl.htmlInner1st = htmlinner1st,
                            Tmpl.tmpl = tmpl
                        }
            (pagevars , pagedate , pagesrcchunks) = pageVars cfgproj pagesrc $outjob.:Build.contentDate
            taghandler = tagHandler cfgproj ctxpage outjob
            tmpl = tmplfinder$ System.FilePath.takeExtension dstfilepath
            pageonlyproc = Tmpl.processSrcFully ctxtmpl (Just ctxpage)
                            (null pagevars |? pagesrc |! (concat pagesrcchunks))
            outsrc = Tmpl.apply tmpl ctxpage pageonlyproc
            htmlinners tagname =
                Html.innerContentsNoAtts Util.trim tagname pageonlyproc
            htmlinner1st tagname defval =
                Util.atOr (htmlinners tagname) 0 defval

        in return (outsrc , mismatches)



pageVars cfgproj pagesrc contentdate =
    (pagevars , pagedate , pagesrcchunks)
    where
    pagevars = chunks >~ foreach ~> Util.unMaybes
    pagedate = Util.atOr pvardates 0 contentdate
    pagesrcchunks = chunks >~ fst ~|is

    foreach (pvarstr , "{:P|") =
        let nameandval = (Util.splitOn1st '=' pvarstr) ~> Util.bothTrim
        in Just nameandval
    foreach _ =
        Nothing

    chunks = (Util.splitUp Util.trim ["{:P|"] "|:}" pagesrc)
    pvardates = chunks >~ maybedate ~> Util.unMaybes
    maybedate (pvdstr,_) =
        let (varname , varval) = Util.bothTrim (Util.splitOn1st '=' pvdstr)
            (dprefix , dtfname) = Util.bothTrim (Util.splitOn1st ':' varname)
        in if dprefix /= "date" then Nothing
            else ProjC.dtStr2Utc cfgproj dtfname varval



tagHandler cfgproj ctxpage outjob tagcontent =
    if tagcontent == "date" then fordate "" contentdate
    else if dtfprefix=="date" then (fordate dtfname contentdate)
    else case Data.List.lookup tagcontent (ctxpage.:Tmpl.pVars) of
        Just val -> Just val
        Nothing -> for tagcontent
    where
    contentdate = ctxpage.:Tmpl.pDate
    (dtfprefix,dtfname) = Util.bothTrim (Util.splitOn1st ':' tagcontent)
    reldir = Util.butNot "." "" (System.FilePath.takeDirectory$ outjob.:Build.relPath)
    reldir' = Util.butNot "." "" (System.FilePath.takeDirectory$ outjob.:Build.relPathSlashes)
    pvals = [ "title" =: (ctxpage.:Tmpl.htmlInner1st) "h1" ""
            , "filePath" =: outjob.:Build.relPath
            , "fileUri" =: '/':(outjob.:Build.relPathSlashes)
            , "fileName" =: (System.FilePath.takeFileName$ outjob.:Build.relPath)
            , "fileBaseName" =: (System.FilePath.takeBaseName$ outjob.:Build.relPath)
            , "dirName" =: Util.ifIs reldir System.FilePath.takeFileName
            , "dirPath" =: Util.ifIs reldir (++[System.FilePath.pathSeparator])
            , "dirUri" =: '/':(Util.ifIs reldir (++"/"))
            , "outPathBuild" =: outjob.:Build.outPathBuild
            , "outPathDeploy" =: outjob.:Build.outPathDeploy
            , "srcFilePath" =: outjob.:Build.srcFile.:Files.path
            ]
    for name =
        let (dtfp,dtfn) = Util.bothTrim (Util.splitOn1st ':' name)
        in if dtfp=="srcFileModTime"
            then fordate dtfn (outjob.:Build.srcFile.:Files.modTime)
            else Data.List.lookup name pvals
    fordate dtfname datetime =
        Just$ ProjC.dtUtc2Str cfgproj dtfname datetime



writeSitemapXml ctxproj buildplan =
    let xmlsitemapfull xmlinneritems =
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
            \<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\">\n\
            \    "++(concat xmlinneritems)++"\n\
            \</urlset>"
        xmlsitemapitem domain relpath moddate priority =
            "<url>\n\
            \        <loc>http://"++domain++"/"++relpath++"</loc>\n\
            \        <lastmod>"++(ProjC.dtUtc2Str (ctxproj.:Proj.setup.:Proj.cfg) "" moddate)++"</lastmod>\n\
            \        <priority>"++(take 3 (show priority))++"</priority>\n\
            \    </url>"
        foreach pageinfo =
            skip |? "" |!
                xmlsitemapitem (ctxproj.:Proj.domainName) relpath (pageinfo.:Build.contentDate) priorel
            where
            skip = (blok/=Bloks.NoBlok) && (not$ blok.:Bloks.inSitemap)
            relpath = pageinfo.:Build.relPathSlashes
            priorel = max 0.0 (priobase - priodown)

            blok = Data.Map.Strict.findWithDefault Bloks.NoBlok (pageinfo.:Build.blokName) (ctxproj.:Proj.setup.:Proj.bloks)
            priodown = 0.1 * (fromIntegral$ (Util.count '/' relpath) + (Util.count '.' relpath) - 1) :: Float
            priobase
                | relpath=="index.html" || relpath=="index.htm"
                = 1.0
                | Util.endsWith relpath "/index.html" || Util.endsWith relpath "/index.htm"
                = 0.88
                | otherwise
                = 0.66
        (outjob , pagefileinfos) = buildplan.:Build.siteMap
        xmlitems = (pagefileinfos >~foreach)
        xmloutput = return (xmlsitemapfull xmlitems , undefined)
    in if outjob == Build.NoOutput
        then return ()
        else Files.writeTo
                (outjob.:Build.outPathBuild)
                (outjob.:Build.relPath)
                xmloutput
            >> return ()
