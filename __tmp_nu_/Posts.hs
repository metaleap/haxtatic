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
                    link="*.haxproj", content = "<pre>" ++ (Html.escape [] parsestr) ++ "</pre>", pic="https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Warning_icon.svg/256px-Warning_icon.svg.png"
                }
        in Just post
    foreach _ =
        Nothing



writeAtoms _ _ _ [] =
    return ()
writeAtoms projbloks projposts projcfg (outjob:more) =
    --  this recursion would seem wasteful with many 1000s of files, but that's unlikely
    Files.writeTo (outjob.:outPathBuild) (outjob.:relPath) xmloutput
    >> writeAtoms projbloks projposts projcfg more
    where

    xmloutput =
        return (xmlatomfull , undefined)

    xmlatomfull =
        let repls = [ "&DOMAIN;" =: projcfg.:ProjC.domainName, "&PAGEURI;" =: pageuri ]
            updated = if null allposts
                        then (ProjC.dtUtc2Str projcfg "" (outjob.:srcFile.:Files.modTime))
                        else ((allposts#0).:dt)
        in Util.replaceSubs repls ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
            \<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
            \    <link rel=\"self\" type=\"application/rss+xml\" href=\"http://&DOMAIN;/"++(urify$ outjob.:relPath)++"\" />\n\
            \    <title>&DOMAIN; "++feedtitle++"</title>\n\
            \    <subtitle>"++(is feedname |? "&DOMAIN;/&PAGEURI;" |! desc)++"</subtitle>\n\
            \    <id>http://&DOMAIN;/&PAGEURI;</id>\n\
            \    <link href=\"http://&DOMAIN;/&PAGEURI;\"/>\n\
            \    <updated>"++updated++"T00:00:00Z</updated>\n\
            \    "++(concat$ allposts >~ xmlatompost)++"\n\
            \</feed>")

    urify = Files.pathSepSystemToSlash
    blokname = outjob.:blokName
    maybeblok = Data.Map.Strict.lookup blokname projbloks
    feedname = is blokname |? "" |! drop 1 (System.FilePath.takeExtension$ outjob.:srcFile.:Files.path)
    allposts = if is blokname
                then [P { feed = blokname, dt="1234-05-06", cat = blokname, title = blokname,
                            link = blokname, pic = blokname, content = blokname }]
                else projposts ~|(\p -> (p.:feed)==feedname)
    pageuri = maybeblok~>((urify . Bloks.blokIndexPageFile) =|- feedname++".html")
    feedtitle = xmlesc$ maybeblok~>(Bloks.title =|- feedname)
    desc = xmlesc$ maybeblok~>(Bloks.desc =|- "")

    xmlesc = Html.escape []
    sanitize = Util.replaceSubs [ "<link " =: "<hax_link style=\"display:none\" " , "<script" =: "<!--hax_script" ,
                "<input " =: "<hax_input style=\"display:none\"" , "</link" =: "</hax_link" ,
                "</script>" =: "</hax_script-->" , "</input" =: "</hax_input" , " style=\"" =: " hax_style=\""
                ]

    xmlatompost post =
        let posttitle = xmlesc (post.:title)
            postdesc = xmlesc (if is blokname then post.:content else post.:cat)
            postfull = (xmlesc.sanitize) (if is blokname then "html here" else post.:content)
            repl = [ "&POSTDATE;" =: post.:dt , "&POSTURL;" =: if is blokname then (post.:link) else "/"++pageuri++"#"++(post.:dt) ]
        in Util.replaceSubs repl ("<entry>\n\
            \        <title type=\"html\">"++posttitle++"</title>\n\
            \        <summary type=\"html\">"++postdesc++"</summary>\n\
            \        <link href=\"&POSTURL;\"/><author><name>"++(post.:cat)++"</name><email>info@&DOMAIN;</email></author>\n\
            \        <id>tag:&DOMAIN;,&POSTDATE;:&POSTURL;</id>\n\
            \        <updated>&POSTDATE;T00:00:00Z</updated>\n\
            \        <content type=\"html\">"++postfull++"</content>\n\
            \    </entry>")
