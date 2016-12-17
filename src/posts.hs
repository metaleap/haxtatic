module Posts where

import qualified Blogs
import qualified Util

import qualified Data.Char

data Post = Post { fn :: Util.FName, subcat :: String, title :: String, link :: String, pic :: String, text :: String, cat :: String, origraw :: String } deriving (Read)



tmplAtom = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
    \    <link rel=\"self\" type=\"application/rss+xml\" href=\"http://&DOMAIN;/&ATOMFILE;\" />\n\
    \    <title>&DOMAIN; &TITLE;</title><subtitle>&DOMAIN;/&PAGENAME;.html</subtitle>\n\
    \    <id>http://&DOMAIN;/&PAGENAME;.html</id>\n\
    \    <link href=\"http://&DOMAIN;/&PAGENAME;.html\"/>\n\
    \    <updated>&DATE;T00:00:00Z</updated>\n\
    \    &ENTRIES;\n\
    \</feed>"

tmplAtomEntry = "<entry>\n\
    \        <title type=\"html\">&POSTTITLE;</title><summary type=\"html\">&POSTDESC;</summary>\n\
    \        <link href=\"&POSTURL;\"/><author><name>&POSTAUTHOR;</name><email>info@&DOMAIN;</email></author>\n\
    \        <id>tag:&DOMAIN;,&POSTDATE;:&POSTURL;</id>\n\
    \        <updated>&POSTDATE;T00:00:00Z</updated>\n\
    \        <content type=\"html\">&POST;</content>\n\
    \    </entry>"




loadPosts fname rawsrc =
    map perline $ filter Util.is $ lines rawsrc where
        perline ln = (read ("Post {"++ln++", cat=\""++fname++"\", origraw=\"\"}") :: Post)


toAtom domain feedname posts bbn = (atomfilename, rawxml) where
    atomfilename = if pblog then (Blogs.atom blog) else feedname++".atom"
    rawxml = if (pblog && (null atomfilename)) then "" else Util.replacein tmplAtom [
            ("&TITLE;", if pblog then repl $ Blogs.title blog else feedname),
            ("&PAGENAME;", if pblog then p0fnn else map Data.Char.toLower feedname),
            ("&ATOMFILE;", Util.replacein atomfilename [(" ","%20")]),
            ("&DATE;", Util.join "-" $ Util.take3 p0fn),
            ("&DOMAIN;", domain),
            ("&ENTRIES;", concat entries)
        ] where
            entries = map perpost posts
            perpost p =
                let pfn = fn p in
                    Util.replacein tmplAtomEntry [
                        ("&POSTTITLE;", repl $ Posts.title p),
                        ("&POSTURL;", if pblog then (link p) else ("&PAGENAME;.html#"++(Util.join "_" pfn))),
                        ("&POSTDATE;", Util.join "-" $ Util.take3 pfn),
                        ("&POSTDESC;", repl $ if pblog then text p else subcat p),
                        ("&POSTAUTHOR;", if pblog then "&DOMAIN;" else subcat p),
                        ("&POST;", repl $ if pblog then origraw p else text p)
                    ]
    blog = bbn p0fnn ; repl s = Util.replacein s [("\"","&quot;"),("'","&apos;"),(">","&gt;"),("<","&lt;"),("&","&amp;"),("<link","<hax_link"),("<script","<!--hax_script"),("<input","<hax_input"),("</link","</hax_link"),("</script>","</hax_script-->"),("</input","</hax_input"),(" style=\""," hax_style=\"")]
    pblog = (length p0fn) > 4 ; p0fn = fn $ head posts ; p0fnn = Util.fnName p0fn
