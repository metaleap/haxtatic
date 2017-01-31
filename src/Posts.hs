{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Posts where

import Base
import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Html
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict
import qualified System.FilePath
import System.FilePath ( (</>) )


data Ctx
    = BuildContext {
        lookupCachedPageRender :: FilePath->Maybe Tmpl.CtxPage,
        allPagesFiles :: [(FilePath , Files.File)],
        projBloks :: Data.Map.Strict.Map String Bloks.Blok,
        projPosts :: [Item],
        projCfg :: ProjC.Config
    }

data Item
    = From {
        dt :: String,
        cat :: String,
        title :: String,
        link :: String,
        more :: Util.StringPairs,
        content :: String,
        feed :: String
    }
    deriving (Eq, Read, Ord)

data Feed =
    BuildTask {
        blokName :: String,
        outPathBuild :: FilePath,
        relPath :: FilePath,
        srcFile :: Files.File
    }


data Query
    = All
    | Some {
        feeds :: [String],
        cats :: [String],
        dates :: QueryDate
    }
    deriving Read

data QueryDate
    = Any
    | Between String String
    deriving Read


buildPlan modtimeproj relpathpostatoms feednames =
    feednames >~ tofileinfo
    where
    tofileinfo feedname =
        ( dstfilerelpath , file )
        where
        file = if null dstfilerelpath then Files.NoFile else
                Files.FileInfo { Files.modTime = modtimeproj , Files.path = ":F|/."++feedname }
        dstfilename = feedname++".atom"
        dstfilerelpath
            |(relpathpostatoms==Defaults.dir_PostAtoms_None)= ""
            |(null relpathpostatoms)= dstfilename
            |(otherwise)= relpathpostatoms </> dstfilename



dtYear post =
    take 4 $post-:dt



feedPosts maybectxbuild projposts projbloks query blokcat morefromhtmls =
    if everything query then allposts else allposts ~| match
    where
    everything All = True ; everything (Some [] [] Any) = True ; everything (Some [] [] (Between "" "")) = True ; everything _ = False
    queryfeeds = Util.noNils (query-:feeds)
    allposts = projposts ++ (concat$ bloknames>~postsfromblok)
    postsfromblok blokname = (postsFromBlok maybectxbuild morefromhtmls blokcat blokname) >~ snd
    bloknames = let bnames = Data.Map.Strict.keys projbloks in
                    if everything query then bnames else bnames ~|(`elem` queryfeeds)
    match post =
        (check feed queryfeeds && check cat (query-:cats) && (checkdate $query-:dates))
            || (post-:cat == "_hax_cat") || (post-:dt) == "9999-12-31"
        where
        check field criteria =
            null criteria || any ((post-:field)==) criteria
        checkdate (Between "" "") =
            True
        checkdate (Between mindate maxdate) =
            (post-:dt) >= mindate && (post-:dt) <= maxdate
        checkdate _ =
            True

feedGroups maybectxbuild projposts projbloks query fieldname =
    Util.unique$ case findfield wellKnownFields of
        Just field -> allposts >~ field
        Nothing -> allposts >=~ (findfield.more)
    where
    findfield = Data.List.lookup fieldname
    allposts = feedPosts maybectxbuild projposts projbloks query "" []



moreFromHtmlFieldName (htmltag , htmlattr) =
    ('<':htmltag) ++ ('>':htmlattr)

moreFromHtmlSplit ('<':morename) =
    let htmltagandattr = Util.splitOn1st_ '>' morename
    in (elem '>' morename) |? Just htmltagandattr |! Nothing
moreFromHtmlSplit _ =
    Nothing



parseProjChunks projcfg chunkssplits =
    (feednames , posts)
    where
    feednames = Util.unique (posts>~feed)
    posts = Data.List.sortBy cmpposts (chunkssplits>=~foreach)
    cmpposts post1 post2 =
        compare (post2-:dt) (post1-:dt)
    foreach (pfeedcat:pvalsplits) =
        let
            pstr = Util.join ":" pvalsplits ~> Util.trim
            parsestr' = (Tmpl.fixParseStr "content" pstr)
            parsestr = "From {" ++ parsestr' ++ ",feed=\"" ++ pfeedcat ++ "\"}"
            post = Util.tryParseOr errpost parsestr
            errpost = From {
                    title = if projcfg-:ProjC.parsingFailEarly
                                then ProjC.raiseParseErr "*.haxproj" ("|P|"++pfeedcat++":") parsestr'
                                else "{!|P| syntax issue, couldn't parse this post |!}",
                    content = "<pre>" ++ (Html.escape pstr) ++ "</pre>",
                    feed = pfeedcat, dt = "9999-12-31", cat = "_hax_cat", link = "*.haxproj", more = []
                }
        in Just post
    foreach _ =
        Nothing



postsFromBlok (Just ctxbuild) morefromhtmls blokcat blokname =
    allblokpages >~ topost
    where
    (allblokpages , cdatelatest) = Bloks.allBlokPageFiles (ctxbuild-:projCfg) (ctxbuild-:allPagesFiles) blokname
    topost (relpath,file) =
        let (htmlcontent , htmlinner1st) = case maybectxpage of
                Nothing -> ( "<p>HaXtatic&apos;s fault: currently can&apos;t embed the requested content here unless `"
                                ++ relpath ++ "` is included in your build.</p>",
                                    \_ _ -> "Include " ++ relpath ++ " in your rebuild" )
                Just ctxpage -> (ctxpage-:Tmpl.cachedRenderSansTmpl , ctxpage-:Tmpl.htmlInner1st)
            maybectxpage = (ctxbuild-:lookupCachedPageRender) $file-:Files.path
            morefromhtml (htmltag , "") =
                htmltag =: htmlfind1st (Html.findInnerContentOfTags htmltag)
            morefromhtml htmlboth@(htmltag , htmlattr) =
                (moreFromHtmlFieldName htmlboth) =: htmlfind1st (Html.findValuesOfVoidTags1stAttr htmltag htmlattr)
            htmlfind1st finder =
                Html.find1st finder "" htmlcontent
            formatdt dtf =
                ProjC.dtUtc2Str (ctxbuild-:projCfg) dtf (maybectxpage-:(Tmpl.pDate =|- cdatelatest))
            dyncat p = case Data.List.lookup blokcat wellKnownFields of
                        Just field -> p-:field
                        Nothing -> Util.lookup blokcat "" (p-:more)
            post = From {
                feed = blokname,
                cat = "",
                dt = formatdt "",
                title = htmlinner1st "h1" relpath,
                link = Files.sanitizeUriRelPathForJoin relpath,
                more = morefromhtmls >~ morefromhtml,
                content = (htmlinner1st "p" htmlcontent) -- Html.stripMarkup ' '
            }
        in (htmlcontent , post { cat = dyncat post })

postsFromBlok _ _ _ _ =
    []



wellKnownFields =
    [ "feed"=:feed, "dt"=:dt, "dt:year"=:dtYear, "cat"=:cat, "title"=:title, "link"=:link, "content"=:content ]



writeAtoms _ _ [] =
    return ()
writeAtoms ctxbuild domainname outjobs =
    outjobs >>~ writeatom >> return ()
    where

    (domain,domainwtf) = Util.splitOn1st '/' domainname
    postsfromblok = postsFromBlok (Just ctxbuild) [] ""

    writeatom outjob =
        Files.writeTo (outjob-:outPathBuild) relpath xmlatomfull >>= \nowarnings
        -> if nowarnings then return ()
            else putStrLn ("...\t<~\tProbable {!| ERROR MESSAGES |!} were rendered into `"++relpath++"`")

        where

        relpath = outjob-:relPath
        srcpath = outjob-:srcFile-:Files.path

        xmlatomfull =
            let updated = if null allposts
                            then (ProjC.dtUtc2Str (ctxbuild-:projCfg) "" (outjob-:srcFile-:Files.modTime))
                            else ((snd$ allposts@!0)-:dt)
                xmlintro = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
                            \<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
                            \    <link rel=\"self\" type=\"application/rss+xml\" href=\"http://"++domainname++"/"++(urify relpath)++"\" />\n\
                            \    <title>"++domain++" "++feedtitle++"</title>\n\
                            \    <subtitle>"++(has feedname |? (domainname++pageuri) |! (Util.trim$ Html.stripMarkup ' ' feeddesc))++"</subtitle>\n\
                            \    <id>http://"++domainname++pageuri++"</id>\n\
                            \    <link href=\"http://"++domainname++pageuri++"\"/>\n\
                            \    <updated>"++updated++"T00:00:00Z</updated>\n    "
                xmlinner = concat$ allposts>~xmlatompost
                nowarn = i1 < 0 || i2 < (i1 + 4) where
                    i1 = Util.indexOfSub xmlinner "{!|"
                    i2 = Util.indexOfSub xmlinner "|!}"
            in return (xmlintro++xmlinner++"\n</feed>" , nowarn)

        root2rel = Html.rootPathToRel relpath
        urify = Files.pathSepSystemToSlash
        blokname = outjob-:blokName
        feedname = has blokname |? "" |!
                    drop 1 (System.FilePath.takeExtension srcpath)
        maybeblok = Bloks.blokByName (ctxbuild-:projBloks) blokname
        allposts = case maybeblok of
                    Nothing -> ((ctxbuild-:projPosts) ~|(==feedname).feed) >~((,) "")
                    Just _ -> postsfromblok blokname
        (pageuri , feedtitle , feeddesc) = case maybeblok of
                    Just blok -> ( '/':(urify (blok-:Bloks.blokIndexPageFile)) , blok-:Bloks.title , blok-:Bloks.desc )
                    Nothing -> ( '/':(feedname++".html") , feedname , "" )
        pageuri' = drop 1 pageuri

        xmlesc = Html.escape
        sanitize = Util.replaceSubsMany ["<link " =: "<hax_link style=\"display:none\" " , "<script" =: "<!--hax_script" ,
                    "<input " =: "<hax_input style=\"display:none\"" , "</link" =: "</hax_link" ,
                    "</script>" =: "</hax_script-->" , "</input" =: "</hax_input" , " style=\"" =: " hax_style=\""
                    ]

        xmlatompost (htmlcontent , post) =
            let posttitle = xmlesc$ Util.ifNo (post-:title) (post-:cat)
                postdesc = xmlesc (post-:(has blokname |? content |! cat))
                postfull = xmlesc (has blokname |? (sanitize htmlcontent) |! (post-:content))
                postdt = post-:dt
                posturl = Util.ifNo (post-:link) (pageuri'++('#':postdt))
            in ("<entry>\n\
                \        <title type=\"html\">"++posttitle++"</title>\n\
                \        <summary type=\"html\">"++postdesc++"</summary>\n\
                \        <link href=\""++((Util.contains posturl "://") |? posturl |! root2rel posturl)++"\"/><author><name>"++domain++"</name></author>\n\
                \        <id>tag:"++domain++(',':postdt)++(':':domainwtf)++('/':posturl)++"</id>\n\
                \        <updated>"++postdt++"T00:00:00Z</updated>\n\
                \        <content type=\"html\">"++postfull++"</content>\n\
                \    </entry>")
