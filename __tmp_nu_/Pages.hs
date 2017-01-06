{-# OPTIONS_GHC -Wall #-}
module Pages where

import Base
import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
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
    in Tmpl.loadAll
                ctxmain
                ctxtmpl
                (ctxproj.:Proj.coreFiles)
                filenameexts
                (ctxproj.:Proj.setup.:Proj.cfg.:ProjC.htmlEquivExts)
        >>= \tmplfinder
    -> let foreach buildtask =
            processPage ctxmain ctxtmpl tmplfinder buildtask
        in buildplan.:Build.outPages >>~ foreach
        >> return ()



processPage ctxmain ctxtmpl tmplfinder outjob =
    Files.writeTo dstfilepath (outjob.:Build.relPath) processcontent
    >>= Tmpl.warnIfTagMismatches ctxmain srcfilepath
    where
    dstfilepath = outjob.:Build.outPathBuild
    srcfilepath = outjob.:Build.srcFile.:Files.path
    processcontent =
        System.IO.hFlush System.IO.stdout
        >> loadsrccontent >>= \(mismatches , pagesrc)
        -> let
            ctxpage = Tmpl.PageContext {
                            Tmpl.blokName = outjob.:Build.blokName,
                            Tmpl.pTags = tagresolver,
                            Tmpl.pVars = pagevars,
                            Tmpl.tmpl = tmpl
                        }
            (pagevars , pagesrcchunks) = pageVars pagesrc
            tagresolver = tagResolver ctxpage
            tmpl = tmplfinder$ System.FilePath.takeExtension dstfilepath
            pageonlyproc = Tmpl.processSrcFully ctxtmpl (Just ctxpage)
                            (null pagevars |? pagesrc |! (concat pagesrcchunks))
            outsrc = Tmpl.apply tmpl ctxpage pageonlyproc
        in return (outsrc , mismatches)
    loadsrccontent =
        let blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
            blokindextmpl = tmplfinder Defaults.blokIndexPrefix
        in if is blokindexname
            then return ((0,0) , blokindextmpl.:Tmpl.srcFile.:Files.content)
            else readFile srcfilepath >>= \rawsrc
                    -> return (Tmpl.tagMismatches rawsrc , rawsrc)



pageVars pagesrc =
    (pagevars , pagesrcchunks)
    where
    chunks = (Util.splitUp ["{:P|"] "|:}" pagesrc)
    pagevars = chunks >~ foreach ~> Util.unMaybes
    pagesrcchunks = chunks >~ fst ~|is
    foreach (pvarstr , "{:P|") =
        let nameandval = (Util.splitOn1st '=' pvarstr) ~> (Util.both' Util.trim)
        in Just nameandval
    foreach _ =
        Nothing



tagResolver ctxpage tagcontent =
    case Data.List.lookup tagcontent (ctxpage.:Tmpl.pVars) of
        Just val -> Just$ "--==WOWLOOKSIE::"++val++"::NOICE==--"
        Nothing ->
            if tagcontent=="Title"
                then Just ("==FOO:"++(ctxpage.:Tmpl.blokName)++":DERE==")
                else Nothing



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
