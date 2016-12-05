module Main where

import Control.Monad
import Data.List
import Data.Time.Clock
import System.Directory
import System.Environment

import Blogs
import Config
import Html
import Pages
import Posts
import Util


helpmsg = "\n=== HAXTATIC ===\nNo project directory path given.\n  For existing project: specify its directory.\n\
    \  For a new project structure: specify its intended directory."


main = do
    -- grab some definitely-needed inputs right now or fail right here
    nowtime <- Data.Time.Clock.getCurrentTime
    cmdargs <- System.Environment.getArgs
    if length cmdargs < 1 then putStrLn helpmsg else
        let dirsite = head cmdargs ; path = Util.fsPath dirsite
            readorcreate file = System.Directory.doesFileExist file >>=
                \isfile -> if isfile then readFile file else writeFile file "" >> return ""
        in do
            System.Directory.createDirectoryIfMissing True dirsite
            filestream_tmplmain <- readorcreate $ path "theme.tmpl.html"
            filestream_tmplblog <- readorcreate $ path "blog.tmpl.html"
            filestream_config <- readorcreate $ path "haxtatic.config"
            let -- first, a few local helper functions utilizing the above inputs:
                dirout = path "build" -- if empty, will be <datetime>
                dirpages = path "pages" -- dirname of source content pages to load & process
                dirposts = path "posts" -- the .posts files to load
                dirstatic = path "static" -- dirname of static files/folders to copy into to outdir
                sitename = (last $ Util.splitBy '/' $ Util.swapout '\\' '/' dirsite)
                cfgexts = Config.exts cfglines ; cfglines = lines filestream_config
                blogs = Config.blogs cfglines ; blognames = map Blogs.name blogs
                splitdot = Util.splitBy '.'

                -- in pages dir, ignore . and .. and pick files named yyyy.mm.dd.*.* (hackily enough, we really pick *.*.*.*.*)
                filterPageFileNames filenames = Control.Monad.filterM ispagefile filenames where
                    ispagefile filename = let srcpath = Util.fsPath dirpages filename in System.Directory.doesFileExist srcpath >>=
                        \isafile -> return $ isafile && (length $ splitdot filename) > 4

                -- knowing actual page files, load posts from all files in 'posts' dir
                loadAllPosts pagefilenames = let
                    ispostfile = (System.Directory.doesFileExist . Util.fsPath dirposts)
                    per_postsfile filename = let srcpath = Util.fsPath dirposts filename in
                        readFile srcpath >>= \rawsrc -> return $ Posts.loadPosts filename (applyprep rawsrc)
                    sortposts = quickSort Posts.fn (>=) (<) -- only sort by date, newest to oldest
                    blogposts = mapM topost $ filter isblogpage pagefilenames where
                        topost pagefilename = let fn = splitdot pagefilename
                            in readFile (Util.fsPath dirpages pagefilename) >>= \rawsrc -> let
                                h1 = Html.tagInner "h1" $ head rawlines ; rawlines = lines raw ; raw = applyprep rawsrc
                                p = if (length ps) > 0 then head ps else ""
                                ps = filter (\l -> Util.is $ tagInner "p" l) rawlines
                                ptext = Html.tagInnerPlain p
                                pcat = Blogs.title (blogbyname (Util.fnName fn))
                                (pname, ptitle, plink) = (fn, h1, Util.join "." (drop3 fn))
                                psubcat = (Util.monthName $ Util.fnMonth fn)++" "++(Util.fnDay fn)
                                in return $ [Posts.Post pname psubcat ptitle plink "" ptext pcat (Pages.processMarkupDumb pname ptitle raw alltemplaters)]
                        isblogpage pfn = Util.isin (Util.fnName $ splitdot pfn) blognames
                    alltemplaters = concat $ map Pages.xTemplaters cfgexts
                    mergePosts allfileposts = blogposts >>= (return . concat . (++)allfileposts)
                    dodem bla = bla
                    toAtoms allposts = map percat cats where
                        cats = Data.List.nub $ map Posts.cat allposts
                        percat cat = Posts.toAtom sitename cat $ filter (\p->(cat==(Posts.cat p))) allposts
                    in System.Directory.doesDirectoryExist dirposts >>=
                        (\isdir ->if isdir then System.Directory.getDirectoryContents dirposts else return []) >>=
                            Control.Monad.filterM ispostfile >>= mapM per_postsfile >>= mergePosts >>=
                                \allposts -> let
                                    atoms = toAtoms allposts ; a0 = head atoms
                                    peratom a = writefile (fst a) (snd a)
                                    pfns = map splitdot pagefilenames
                                    in do
                                        mapM peratom atoms
                                        writefile "sitemap.xml" (Pages.toSitemap sitename pfns blognames)
                                        return $ map (\pfn -> (pfn, sortposts allposts, alltemplaters)) pagefilenames
                blogbyname bn = head $ filter (\b -> (Blogs.name b) == bn) blogs
                writefile fn c = putStr ("\t"++fn++".. ") >> writeFile (Util.fsPath dirout fn) c >> putStrLn "OK"

                -- read a page file and create a Pages.Page from its raw source
                per_pagesrcfile (pagefilename,allposts,alltemplaters) =
                    readFile (Util.fsPath dirpages pagefilename) >>=
                        \rawsrc -> return $ pagefromsrc allposts alltemplaters (splitdot pagefilename) (applyprep rawsrc)

                -- given file-name info and raw page source, create a Pages.Page "ready for applyTemplate"
                pagefromsrc = Pages.newPage nowint cfgexts
                nowint = floor $ toRational $ Data.Time.Clock.utctDayTime nowtime

                -- given list of Pages.Page (plus generating one more on-the-fly for the blog-index page),
                -- apply template and generate final output page file for each
                genpages pages =
                    mapM_ per_page allpages where
                        allpages = if ((length bpages) < 1) then pages else (blogindexpages ++ pages)
                        per_page page = applyTemplate page >>= writefile (outfilename page)
                        bpages = filter isblog pages where isblog page = (Util.isin (Util.fnName $ Pages.fname page) blognames)
                        outfilename page = Util.join "." $ drop3 $ Pages.fname page
                        blogindexpages = map blogindexpage blognames
                        blogindexpage bname =
                            pagefromsrc allposts alltemplaters [year,month,day,bname,"html"] (applyprep_ bname filestream_tmplblog) where
                                year = Util.fnYear bpfn ; month = Util.fnMonth bpfn ; day = Util.fnDay bpfn
                                (allposts,alltemplaters) = (Pages.allPosts fstpage , Pages.allXTemplaters fstpage)
                                fstpage = head pages ; bph = head bps ; bps = (filter (\bp -> bname == (Util.fnName $ Pages.fname bp)) bpages)
                                bpfn = if (length bps) > 0 then Pages.fname bph else ["0000","00","00"]

                -- given a Pages.Page (and list of all Pages.Page for blog posts), render final output-page content
                applyTemplate page =
                    return $ unlines $ map per_line (lines (applyprep filestream_tmplmain)) where
                        navname = Util.fnName $ Pages.fname page ; titles = Pages.titles page
                        per_line "{{P:Body}}" = unlines $ Pages.body page
                        per_line ln = concat $ Pages.processMarkupLn page ln
                applyprep = applyprep_ "" ; applyprep_ = Blogs.tmplMarkupSrc blogs

                in -- now let's go!
                    mapM (System.Directory.createDirectoryIfMissing False) [dirout, dirpages, dirposts, dirstatic]
                    >> putStrLn ("\n=== HAXTATIC ===\nCopying everything inside '"++dirstatic++"'\nover into '"++dirout++"'..")
                    >> System.Directory.getDirectoryContents dirstatic >>= copyFiles dirstatic dirout
                    >> putStrLn ("\tOK\nLoading from '"++dirpages++"' & '"++dirposts++"'\nand generating in '"++dirout++"'..")
                    >> System.Directory.getDirectoryContents dirpages
                    >>= filterPageFileNames
                    >>= loadAllPosts
                    >>= mapM per_pagesrcfile
                    >>= genpages
