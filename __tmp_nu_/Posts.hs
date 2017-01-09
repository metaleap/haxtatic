{-# OPTIONS_GHC -Wall #-}
module Posts where

import Base
import qualified Bloks
import qualified Defaults
import qualified Files
import qualified Html
import qualified ProjC
import qualified Util

import qualified Data.List
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
        let dstfilename = feedname++".atom"
            dstfilerelpath
                |(relpathpostatoms==Defaults.dir_PostAtoms_None)= ""
                |(null relpathpostatoms)= dstfilename
                |(otherwise)= relpathpostatoms </> dstfilename
        in ( dstfilerelpath , if null dstfilerelpath then Files.NoFile
                                else Files.FileInfo {
                                            Files.path = Defaults.feedIndexPrefix++"/"++"."++feedname,
                                            Files.modTime = modtimeproj
                                        } )



parseProjChunks chunkssplits =
    (feeds , posts)
    where
    feeds = Util.unique (posts>~feed)
    posts = Data.List.sortBy cmpposts (chunkssplits>~foreach ~> Util.unMaybes)
    cmpposts post1 post2 =
        compare (post2.:dt) (post1.:dt)
    foreach (pfeedcat:pvalsplits) =
        let
            pstr = Util.join ":" pvalsplits ~> Util.trim
            parsestr = ((("P {feed = \"" ++ pfeedcat ++ "\", ") ++).(++"}"))$
                        if i < 0 then pstr else
                        (take i pstr) ++ "content=" ++ (pstr ~> ((drop$ i+l) ~. Util.trim ~. show))
                        where i = Util.indexOfSub pstr "content::" ; l = 11 -- "content::"~>length
            tmp = ( pfeedcat ~> Util.trim , pstr )
            post = Util.tryParseOr errpost parsestr
            errpost = P {
                    feed=pfeedcat,dt="9999-12-31", cat="_hax_cat", title="{!| syntax issue, couldn't parse this post |!}",
                    link="*.haxproj", content = "<pre>" ++ (Html.escape parsestr) ++ "</pre>",
                    pic="https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Warning_icon.svg/256px-Warning_icon.svg.png"
                }
        in Just post
    foreach _ =
        Nothing



postsFromBlok projcfg allpagesfiles blokname blok =
    allblokpages >>~ topost
    where
    (allblokpages , cdatelatest) = Bloks.allBlokPageFiles projcfg allpagesfiles blokname
    topost (relpath,file) =
        let
            relpageuri = Files.pathSepSystemToSlash relpath
        in readFile (file.:Files.path) >>= \htmlcontent
        -> return (htmlcontent , P {
                feed = blokname,
                dt="1234-05-06",
                cat = blokname,
                title = relpath,
                link = relpageuri,
                pic = show (file.:Files.modTime),
                content = (file.:Files.path)
            })



writeAtoms _ _ _ _ [] =
    return ()
writeAtoms allpagesfiles projbloks projposts projcfg outjobs =
    outjobs >>~ writeatom >> return ()
    where

    domainname = projcfg.:ProjC.domainName
    postsfromblok = postsFromBlok projcfg allpagesfiles

    writeatom outjob =
        Files.writeTo (outjob.:outPathBuild) relpath xmlatomfull
        where

        relpath = outjob.:relPath
        srcpath = outjob.:srcFile.:Files.path

        xmlatomfull =
            getposts >>= \allposts ->
                let updated = if null allposts
                                then (ProjC.dtUtc2Str projcfg "" (outjob.:srcFile.:Files.modTime))
                                else ((snd$ allposts#0).:dt)
                    xmlintro = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
                                \<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
                                \    <link rel=\"self\" type=\"application/rss+xml\" href=\"http://"++domainname++"/"++(urify relpath)++"\" />\n\
                                \    <title>"++domainname++" "++feedtitle++"</title>\n\
                                \    <subtitle>"++(is feedname |? (domainname++pageuri) |! desc)++"</subtitle>\n\
                                \    <id>http://"++domainname++pageuri++"</id>\n\
                                \    <link href=\"http://"++domainname++pageuri++"\"/>\n\
                                \    <updated>"++updated++"T00:00:00Z</updated>\n    "
                in
                    return (concat$ (xmlintro:(allposts >~ xmlatompost))++["\n</feed>"] , ())

        urify = Files.pathSepSystemToSlash
        blokname = outjob.:blokName
        feedname = is blokname |? "" |!
                    drop 1 (System.FilePath.takeExtension srcpath)
        maybeblok = is blokname |? (Bloks.blokByName projbloks blokname) |! Nothing
        getposts = case maybeblok of
                    Nothing -> return ( (projposts ~|(==feedname).feed) >~((,) "") )
                    Just blok -> postsfromblok blokname blok
        (pageuri , feedtitle , desc) = case maybeblok of
                    Just blok -> ( '/':(urify (blok.:Bloks.blokIndexPageFile)) , blok.:Bloks.title , blok.:Bloks.desc )
                    Nothing -> ( '/':(feedname++".html") , feedname , "" )

        xmlesc = Html.escape
        sanitize = Util.replaceSubs ["<link " =: "<hax_link style=\"display:none\" " , "<script" =: "<!--hax_script" ,
                    "<input " =: "<hax_input style=\"display:none\"" , "</link" =: "</hax_link" ,
                    "</script>" =: "</hax_script-->" , "</input" =: "</hax_input" , " style=\"" =: " hax_style=\""
                    ]

        xmlatompost (htmlcontent , post) =
            let posttitle = xmlesc (post.:title)
                postdesc = xmlesc (post.:(is blokname |? content |! cat))
                postfull = xmlesc (is blokname |? (sanitize htmlcontent) |! (post.:content))
                postdt = post.:dt
                posturl = is blokname |? post.:link |! pageuri++"#"++postdt
            in ("<entry>\n\
                \        <title type=\"html\">"++posttitle++"</title>\n\
                \        <summary type=\"html\">"++postdesc++"</summary>\n\
                \        <link href=\""++posturl++"\"/><author><name>"++domainname++"</name></author>\n\
                \        <id>tag:"++domainname++","++postdt++":"++posturl++"</id>\n\
                \        <updated>"++postdt++"T00:00:00Z</updated>\n\
                \        <content type=\"html\">"++postfull++"</content>\n\
                \    </entry>")
