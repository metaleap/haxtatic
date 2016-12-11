module Posts where

import Blogs
import Data.Char
import Data.List.Utils

import Util

data Post = Post { fn :: Util.FName, subcat :: String, title :: String, link :: String, pic :: String, text :: String, cat :: String, origraw :: String } deriving (Read)



tmplAtom = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
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


toAtom domain feedname posts bbn = (feedname++".atom",rawxml) where
    rawxml = if (pblog && (not $ Blogs.atom $ bbn p0fnn)) then "" else Util.replace tmplAtom [
            ("&TITLE;", feedname),
            ("&PAGENAME;", if pblog then p0fnn else map Data.Char.toLower feedname),
            ("&DATE;", Util.join "-" $ take3 p0fn),
            ("&DOMAIN;", domain),
            ("&ENTRIES;", concat entries)
        ] where
            pblog = (length p0fn) > 4 ; p0fn = fn $ head posts ; p0fnn = Util.fnName p0fn ; entries = map perpost posts
            perpost p = let pfn = fn p ; repl = Data.List.Utils.replace "&" "&amp;" in
                Util.replace tmplAtomEntry [
                    ("&POSTTITLE;", repl $ Posts.title p),
                    ("&POSTURL;", if pblog then (link p) else ("&PAGENAME;.html#"++(Util.join "_" pfn))),
                    ("&POSTDATE;", Util.join "-" $ take3 pfn),
                    ("&POSTDESC;", repl $ if pblog then text p else subcat p),
                    ("&POSTAUTHOR;", if pblog then "&DOMAIN;" else subcat p),
                    ("&POST;", repl $ if pblog then origraw p else text p)
                ]
