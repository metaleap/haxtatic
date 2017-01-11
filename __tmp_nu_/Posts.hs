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



buildPlan modtimeproj relpathpostatoms feeds =
    feeds >~ tofileinfo
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



parseProjChunks chunkssplits =
    (feeds , posts)
    where
    feeds = Util.unique (posts>~feed)
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



postsFromBlok pagerendercache projcfg allpagesfiles blokname getcat =
    allblokpages >~ topost
    where
    (allblokpages , cdatelatest) = Bloks.allBlokPageFiles projcfg allpagesfiles blokname
    topost (relpath,file) =
        let
            relpageuri = '/':(Files.pathSepSystemToSlash relpath)
            ctxmaybe = Data.Map.Strict.lookup (file-:Files.path) pagerendercache
            (htmlcontent , htmlinner1st) = case ctxmaybe of
                Nothing -> ("<h1>Well now..</h1><p>..<i>there&apos;s</i> a bug in your static-site generator!</p>",
                                const)
                Just ctxpage -> (ctxpage-:Tmpl.cachedRenderSansTmpl , ctxpage-:Tmpl.htmlInner1st)
            pcat = getcat post
            post = P {
                feed = blokname,
                dt = ProjC.dtUtc2Str projcfg "" (ctxmaybe-:(Tmpl.pDate =|- cdatelatest)),
                cat = pcat,
                title = htmlinner1st "h1" relpath,
                link = relpageuri,
                pic = Html.find1st (Html.findValuesOfVoidTags1stAttr "img" "src") "" htmlcontent,
                content = Html.stripMarkup ' ' (htmlinner1st "p" htmlcontent)
            }
        in (htmlcontent , post)



writeAtoms _ _ _ _ _ [] =
    return ()
writeAtoms pagerendercache allpagesfiles projbloks projposts projcfg outjobs =
    outjobs >>~ writeatom >> return ()
    where

    domainname = projcfg-:ProjC.domainName
    postsfromblok = postsFromBlok pagerendercache projcfg allpagesfiles

    writeatom outjob =
        Files.writeTo (outjob-:outPathBuild) relpath xmlatomfull >>= \nowarnings
        -> if nowarnings then return ()
            else putStrLn ("...\t<<\tProbable {!| ERROR MESSAGES |!} were rendered into `"++relpath++"`")

        where

        relpath = outjob-:relPath
        srcpath = outjob-:srcFile-:Files.path

        xmlatomfull =
            let updated = if null allposts
                            then (ProjC.dtUtc2Str projcfg "" (outjob-:srcFile-:Files.modTime))
                            else ((snd$ allposts~@0)-:dt)
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
        maybeblok = is blokname |? (Bloks.blokByName projbloks blokname) |! Nothing
        allposts = case maybeblok of
                    Nothing -> (projposts ~|(==feedname).feed) >~((,) "")
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
