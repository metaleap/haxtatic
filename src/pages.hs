module Pages where

import qualified Html
import qualified Posts
import qualified Util

import qualified Data.List

data Ctx = Ctx { fname :: Util.FName, titles :: [String], body :: String, origpath :: String, now :: Int, pageVars :: Util.KeyVals, daters :: [(String,[String]->String)], allExts :: [X], allPosts :: [Posts.Post], allXTemplaters :: [Tmpl] }
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




newPageContext now daters allexts allposts alltemplaters fname rawsrc origpath = let
    page = Ctx fname titles bodydone origpath now vars daters allexts allposts alltemplaters
    bodydone = postProcessMarkup page (unlines bodytmp)
    (titles,bodytmp,vars) = foldr accum ([],[],[]) $ map perline $ lines rawsrc
    accum (tlines,blines,vlines) (tline,bline,vline) = (tlines++tline , blines++bline,vlines++vline)
    perline ln = (tline , bline, vline) where
        bline = if null vline then processMarkupLn page ln else []
        tline = concat $ map (istitle . Util.trimSpace) bline
        vline = let splits = Util.splitBy ':' (drop 4 $ take ((length ln)-2) ln)
                    vname = head splits ; vval = Util.trimSpace $ Util.join ":" (tail splits)
                    in if ispvar ln && (length splits)>=2 && Util.is vname && Util.is vval
                        then [("{P{%"++vname++"}}",vval)] else []
        --  another abomination that really shouldn't exist:
        istitle "" = []
        istitle bln = let h1 = Html.tagInner "h1" bln ; h2 = Html.tagInner "h2" bln ; h2_ = Html.tagInner3 "h2" bln in
            if Util.is h2 then [h2] else if Util.is h1 then [h1] else
                if Util.is h2_ then [drop (1+(max 0 $ Util.indexOf '>' h2_)) h2_] else
                    let endpos = Util.indexOfSub "</h2>" bln in if endpos<5 then [] else [drop (1+(Util.indexOf '>' bln)) $ take endpos bln]
    in page


ispvar s = ends s && begins s where
    begins = Data.List.isPrefixOf "{P{%" ; ends = Data.List.isSuffixOf "}}"


processMarkupDumbly4Feeds fn bn pbn title rawsrc origpath alltemplaters daters =
    let blines = lines $ repl rawsrc ; perline l = map repl (processMarkupLn ctxtmp l)
        ctxtmp = Ctx fn [title] (unlines blines) origpath 0 [] daters [] [] alltemplaters
        repl s = Util.replaceIn (if (ispvar s) then "<!--\n"++s++"\n-->" else s) [("{P{BaseName}}",pbn),("{B{Name:_}}",bn),("{B{Title:_}}", "{B{Title:"++(Util.fnName fn)++"}}"),("{B{Desc:_}}", "{B{Desc:"++(Util.fnName fn)++"}}")]
        in postProcessMarkup ctxtmp $ unlines $ concat (map perline blines)


postProcessMarkup page src =
    Util.replaceIn src ([
            ("{P{Title}}", head $ titles page),
            ("{P{OrigPath}}", origpath page),
            ("{P{FileName}}", pfname),
            ("{P{BaseName}}", if lfn>2 then _join $ take (lfn-2) (tail pfn) else head pfn),
            ("{P{PostText}}", if post>=0 then (Posts.text ((allPosts page)!!post)) else ""),
            ("{B{Name:_}}", if lfn>2 then head pfn else ""),
            ("{B{Title:_}}", if lfn>2 then "{B{Title:"++(head pfn)++"}}" else ""),
            ("{B{Desc:_}}", if lfn>2 then "{B{Desc:"++(head pfn)++"}}" else "")
        ] ++ dateformatters ++ (pageVars page)) where
            lfn = length pfn
            pfn = Util.drop3 (fname page) ; pfname = _join pfn ; post = Util.indexIf (((==) pfname) . Posts.link) (allPosts page)
            dateformatters = map formatter (daters page) where
                formatter (name,func) = ("{P{Date"++name++"}}", func $ fname page)


processMarkupLn page lin = concat $ map checkh2' (doline lin) where
    doline ln = lines $ unlines $ map per_token $ Util.splitUp (map reverse ["{X{"]) "}}" ln
    per_token (s,i) = if null i then s else
        concat $ xapply s -- ugliest of hacks! by now it'd really be time for a rewrite..
    checkh2 ln = if null h2 then ln else Html.out "h2" [("", h2),("id",h2innerToId h2)] [] where
        h2 = Html.tagInner "h2" ln
    checkh2' ln = [checkh2 ln]
    xapply ln = let
        tagfull = Util.splitBy ':' ln
        tagname = head tagfull ; tagargs = Util.join ":" $ tail tagfull
        apply = \tmpl -> let ttag = head (Util.splitBy ':' $ tmplTag tmpl) in
            if tagname/=ttag then [] else tmplApply tmpl tagname tagargs page
        in concat $ map apply $ allXTemplaters page


h2innerToId h2 =
    Util.replaceIn h2 Html.escapes


toSitemap domain pagefns blognames exclnames =
    Util.replaceIn tmplSitemap [
            ("&DOMAIN;", domain),
            ("&URLS;", concat (map perpfn $ (++) (filter isincluded pagefns) (blogpfns blognames)))
        ] where
            blogpfns [] = []
            blogpfns (bname:rest) = let
                    dorest = blogpfns rest
                    blogmatches = filter (\pfn -> (bname==(Util.fnName pfn)) && (isincluded pfn)) pagefns
                in if (length blogmatches)==0 then dorest else
                    ((Util.take3 (head blogmatches))++[bname,"html"]):dorest
            isincluded pfn = notElem (Util.fnName pfn) exclnames
            perpfn pfn =
                Util.replaceIn tmplSitemapUrl [
                    ("&DATE;", Util.join "-" $ Util.take3 pfn),
                    ("&FILENAME;", _join pfn3),
                    ("&PRIORITY;", priority pfn3)
                ] where pfn3 = Util.drop3 pfn
            priority [] = "0.0" ; priority ["index",_] = "1.0" ; priority ["default",_] = "0.9"
            priority [n,_] = let b = bidx n in if b<0 then "0.8" else Util.take3 (show (0.8-((fromIntegral b)/10.0)))
            priority (lh:lt) = let b = bidx lh in Util.take3 (show ((1.0/(fromIntegral (length lt)))-((fromIntegral b)/10.0)))
            bidx n = 1+(Util.indexOf n blognames)
