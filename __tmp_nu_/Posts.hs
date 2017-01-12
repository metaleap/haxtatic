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
        pageRenderCache :: Maybe (Data.Map.Strict.Map FilePath Tmpl.CtxPage),
        allPagesFiles :: [(FilePath , Files.File)],
        projBloks :: Data.Map.Strict.Map String Bloks.Blok,
        projPosts :: [Posts.Post],
        projCfg :: ProjC.Config
    }

data Post
    = P {
        feed :: String,
        dt :: String,
        cat :: String,
        title :: String,
        link :: String,
        pic :: String,
        content :: String
    } deriving (Eq, Read)

data Feed =
    Job {
        blokName :: String,
        outPathBuild :: FilePath,
        relPath :: FilePath,
        srcFile :: Files.File
    }


data Query =
    Filter {
        feeds :: [String],
        cats :: [String],
        dates :: Maybe (String , String)
    } deriving (Read)



buildPlan modtimeproj relpathpostatoms feednames =
    feednames >~ tofileinfo
    where
    tofileinfo feedname =
        ( dstfilerelpath , file )
        where
        file = if null dstfilerelpath then Files.NoFile else
                Files.FileInfo { Files.modTime = modtimeproj , Files.path = Defaults.feedIndexPrefix++"/"++"."++feedname }
        dstfilename = feedname++".atom"
        dstfilerelpath
            |(relpathpostatoms==Defaults.dir_PostAtoms_None)= ""
            |(null relpathpostatoms)= dstfilename
            |(otherwise)= relpathpostatoms </> dstfilename



dtYear post =
    take 4 $post-:dt



feedPosts ctxbuild projposts projbloks maybequery =
    case maybequery of
        Nothing -> allposts
        Just query -> allposts ~| match query
    where
    allposts = projposts ++ (concat$ bloknames>~postsfromblok)
    postsfromblok blokname = (postsFromBlok ctxbuild blokname (const "getcathere in Posts.feedPosts")) >~ snd
    bloknames = case maybequery of
        Nothing -> Data.Map.Strict.keys projbloks
        Just query -> (Data.Map.Strict.keys projbloks) ~|(`elem` (query-:feeds))
    match (Filter [] [] Nothing) _ =
        True
    match query post =
        (check feed (query-:feeds) && check cat (query-:cats) && (checkdate $query-:dates))
            || (post-:cat == "_hax_cat") || (post-:dt) == "9999-12-31"
        where
        check field criteria =
            null criteria || any ((post-:field)==) criteria
        checkdate (Just (mindate,maxdate)) =
            (post-:dt) >= mindate && (post-:dt) <= maxdate
        checkdate _ =
            True

feedGroups ctxbuild projposts projbloks maybequery postfield =
    Util.unique (allposts >~ postfield)
    where
    allposts = feedPosts ctxbuild projposts projbloks maybequery



parseProjChunks chunkssplits =
    (feednames , posts)
    where
    feednames = Util.unique (posts>~feed)
    posts = Data.List.sortBy cmpposts (chunkssplits>~foreach ~> Util.unMaybes)
    cmpposts post1 post2 =
        compare (post2-:dt) (post1-:dt)
    foreach (pfeedcat:pvalsplits) =
        let
            pstr = Util.join ":" pvalsplits ~> Util.trim
            parsestr = ("P {feed = \"" ++ pfeedcat ++ "\", ") ++ (Tmpl.fixParseStr "content" pstr) ++ "}"
            post = Util.tryParseOr errpost parsestr
            errpost = P {
                    feed=pfeedcat,dt="9999-12-31", cat="_hax_cat", title="{!|P| syntax issue, couldn't parse this post |!}",
                    link="*.haxproj", content = "<pre>" ++ (Html.escape pstr) ++ "</pre>",
                    pic="https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Warning_icon.svg/256px-Warning_icon.svg.png"
                }
        in Just post
    foreach _ =
        Nothing



postsFromBlok ctxbuild blokname getcat =
    allblokpages >~ topost
    where
    (allblokpages , cdatelatest) = Bloks.allBlokPageFiles (ctxbuild-:projCfg) (ctxbuild-:allPagesFiles) blokname
    topost (relpath,file) =
        let
            relpageuri = '/':(Files.pathSepSystemToSlash relpath)
            ctxmaybe = case (ctxbuild-:pageRenderCache) of
                Nothing -> Nothing
                Just pagerendercache -> Data.Map.Strict.lookup (file-:Files.path) pagerendercache
            (htmlcontent , htmlinner1st) = case ctxmaybe of
                Nothing -> ("<h1>Well now..</h1><p>..<i>there&apos;s</i> a bug in your static-site generator!</p>",
                                const)
                Just ctxpage -> (ctxpage-:Tmpl.cachedRenderSansTmpl , ctxpage-:Tmpl.htmlInner1st)
            pcat = getcat post
            post = P {
                feed = blokname,
                dt = ProjC.dtUtc2Str (ctxbuild-:projCfg) "" (ctxmaybe-:(Tmpl.pDate =|- cdatelatest)),
                cat = pcat,
                title = htmlinner1st "h1" relpath,
                link = relpageuri,
                pic = Html.find1st (Html.findValuesOfVoidTags1stAttr "img" "src") "" htmlcontent,
                content = Html.stripMarkup ' ' (htmlinner1st "p" htmlcontent)
            }
        in (htmlcontent , post)



wellKnownFields True =
    ("dt:year"=:dtYear) : (wellKnownFields False)

wellKnownFields _ =
    [ "feed"=:feed, "dt"=:dt, "cat"=:cat, "title"=:title, "link"=:link, "pic"=:pic, "content"=:content ]



writeAtoms _ [] =
    return ()
writeAtoms ctxbuild outjobs =
    outjobs >>~ writeatom >> return ()
    where

    domainname = ctxbuild-:projCfg-:ProjC.domainName
    postsfromblok = postsFromBlok ctxbuild

    writeatom outjob =
        Files.writeTo (outjob-:outPathBuild) relpath xmlatomfull >>= \nowarnings
        -> if nowarnings then return ()
            else putStrLn ("...\t<<\tProbable {!| ERROR MESSAGES |!} were rendered into `"++relpath++"`")

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
                            \    <title>"++domainname++" "++feedtitle++"</title>\n\
                            \    <subtitle>"++(is feedname |? (domainname++pageuri) |! desc)++"</subtitle>\n\
                            \    <id>http://"++domainname++pageuri++"</id>\n\
                            \    <link href=\"http://"++domainname++pageuri++"\"/>\n\
                            \    <updated>"++updated++"T00:00:00Z</updated>\n    "
                xmlinner = concat$ allposts >~ xmlatompost
                nowarn = i1 < 0 || i2 < (i1 + 4) where
                    i1 = Util.indexOfSub xmlinner "{!|"
                    i2 = Util.indexOfSub xmlinner "|!}"
            in return (xmlintro++xmlinner++"\n</feed>" , nowarn)

        urify = Files.pathSepSystemToSlash
        blokname = outjob-:blokName
        feedname = is blokname |? "" |!
                    drop 1 (System.FilePath.takeExtension srcpath)
        maybeblok = is blokname |? (Bloks.blokByName (ctxbuild-:projBloks) blokname) |! Nothing
        allposts = case maybeblok of
                    Nothing -> ((ctxbuild-:projPosts) ~|(==feedname).feed) >~((,) "")
                    Just _ -> postsfromblok blokname (const blokname)
        (pageuri , feedtitle , desc) = case maybeblok of
                    Just blok -> ( '/':(urify (blok-:Bloks.blokIndexPageFile)) , blok-:Bloks.title , blok-:Bloks.desc )
                    Nothing -> ( '/':(feedname++".html") , feedname , "" )

        xmlesc = Html.escape
        sanitize = Util.replaceSubs ["<link " =: "<hax_link style=\"display:none\" " , "<script" =: "<!--hax_script" ,
                    "<input " =: "<hax_input style=\"display:none\"" , "</link" =: "</hax_link" ,
                    "</script>" =: "</hax_script-->" , "</input" =: "</hax_input" , " style=\"" =: " hax_style=\""
                    ]

        xmlatompost (htmlcontent , post) =
            let posttitle = xmlesc (post-:title)
                postdesc = xmlesc (post-:(is blokname |? content |! cat))
                postfull = xmlesc (is blokname |? (sanitize htmlcontent) |! (post-:content))
                postdt = post-:dt
                posturl = is blokname |? post-:link |! pageuri++"#"++postdt
            in ("<entry>\n\
                \        <title type=\"html\">"++posttitle++"</title>\n\
                \        <summary type=\"html\">"++postdesc++"</summary>\n\
                \        <link href=\""++posturl++"\"/><author><name>"++domainname++"</name></author>\n\
                \        <id>tag:"++domainname++","++postdt++":"++posturl++"</id>\n\
                \        <updated>"++postdt++"T00:00:00Z</updated>\n\
                \        <content type=\"html\">"++postfull++"</content>\n\
                \    </entry>")
