module Pages where

import Html
import Posts
import Util

data Ctx = Ctx { fname :: Util.FName, titles :: [String], body :: [String], bodyLen :: Int, now :: Int, daters :: [(String,[String]->String)], allExts :: [X], allPosts :: [Posts.Post], allXTemplaters :: [Tmpl] }
data Tmpl = Tmpl { tmplTag :: String, tmplApply :: String -> String -> Ctx -> [String] }
data X = X { xTemplaters :: [Tmpl] }


_join = Util.join "."


tmplSitemap = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
    \<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\">\n\
    \    &URLS;\n\
    \</urlset>"

tmplSitemapUrl = "<url>\n\
    \        <loc>http://&DOMAIN;/&FILENAME;</loc>\n\
    \        <lastmod>&DATE;</lastmod>\n\
    \        <priority>&PRIORITY;</priority>\n\
    \    </url>"




newPageContext now daters allexts allposts alltemplaters fname rawsrc = let
    page = Ctx fname titles body (length rawsrc) now daters allexts allposts alltemplaters
    (titles,body) = foldl accum ([],[]) $ map perline $ lines rawsrc
    accum (tlines,blines) (tline,bline) = (tlines++tline , blines++bline)
    perline ln = (tline , bline) where
        bline = processMarkupLn page ln
        tline = concat $ map istitle bline
        istitle bln = let h1 = Html.tagInner "h1" bln ; h2 = Html.tagInner "h2" bln ; h2_ = Html.tagInner3 "h2" bln in
            if Util.is h2 then [h2] else if Util.is h1 then [h1] else if Util.is h2_ then [drop (1+(max 0 $ Util.indexof '>' h2_)) h2_] else []
    in page


processMarkupDumb fn bn pbn title rawsrc alltemplaters daters =
    let blines = lines $ repl rawsrc ; perline l = map repl (processMarkupLn ptmp l)
        ptmp = Ctx fn [title] blines (length rawsrc) 0 daters [] [] alltemplaters
        repl s = Util.replace s [("{{B:Name:}}",bn),("{{P:BaseName}}",pbn)]
        in unlines $ concat (map perline blines)


processMarkupLn page lin = concat $ map checkh2 (doline $ preline lin) where
    checkh2 ln = if null h2 then [ln] else [Html.out "h2" [("", h2),("id",h2)] []] where
        h2 = Html.tagInner "h2" ln
    doline ln = let ll = length ln in
        if not $ ("{{X:"==take 4 ln) && ("}}"==drop (ll-2) ln) then [ln] else let
            tagfull = Util.splitBy ':' $ take (ll-6) $ drop 4 ln
            tagname = head tagfull ; tagargs = Util.join ":" $ tail tagfull
            apply = \tmpl -> let ttag = head (Util.splitBy ':' $ tmplTag tmpl) in
                if tagname/=ttag then [] else tmplApply tmpl tagname tagargs page
            in concat $ map apply $ allXTemplaters page
    preline ln = Util.replace ln ([
            ("{{P:Title}}", head $ titles page),
            ("{{P:FileName}}", pfname),
            ("{{P:PostDesc}}", if post>=0 then (Posts.text ((allPosts page)!!post)) else ""),
            ("{{P:BaseName}}", if lfn>2 then _join $ take (lfn-2) (tail pfn) else head pfn),
            ("{{B:Name:}}", if lfn>2 then head pfn else "")
        ] ++ dateformatters) where
            lfn = length pfn
            pfn = Util.drop3 (fname page) ; pfname = _join pfn ; post = Util.indexif (((==) pfname) . Posts.link) (allPosts page)
    dateformatters = map formatter (daters page) where
        formatter (name,func) = ("{{P:Date"++name++"}}", func $ fname page)



toSitemap domain pagefns blognames exclnames =
    Util.replace tmplSitemap [
            ("&DOMAIN;", domain),
            ("&URLS;", concat (map perpfn $ (++) (filter isincluded pagefns) (blogpfns blognames)))
        ] where
            blogpfns [] = []
            blogpfns (bname:rest) = let
                    dorest = blogpfns rest
                    blogmatches = filter (\pfn -> (bname==(Util.fnName pfn)) && (isincluded pfn)) pagefns
                in if (length blogmatches)==0 then dorest else
                    ((take3 (head blogmatches))++[bname,"html"]):dorest
            isincluded pfn = not $ Util.isin (Util.fnName pfn) exclnames
            perpfn pfn =
                Util.replace tmplSitemapUrl [
                    ("&DATE;", Util.join "-" $ take3 pfn),
                    ("&FILENAME;", _join pfn3),
                    ("&PRIORITY;", priority pfn3)
                ] where pfn3 = drop3 pfn
            priority [] = "0.0" ; priority ["index",_] = "1.0" ; priority ["default",_] = "0.9"
            priority [n,_] = let b = bidx n in if b<0 then "0.8" else take3 (show (0.8-((fromIntegral b)/10.0)))
            priority (lh:lt) = let b = bidx lh in take3 (show ((1.0/(fromIntegral (length lt)))-((fromIntegral b)/10.0)))
            bidx n = 1+(Util.indexof n blognames)
