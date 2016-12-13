module Main where

import Control.Monad
import Data.List
import Data.List.Utils
import Data.Time.Clock
import System.Directory
import System.Environment
import System.FilePath

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
    let numargs = length cmdargs in if numargs < 1 then putStrLn helpmsg else
        let dirsite = head cmdargs ; path = (</>) dirsite
            pagesnositemap = splitarg 2 ; pagesskip = splitarg 3 ; pagesonly = splitarg 4
            skipstaticfolders = numargs>1 && ("True"==(cmdargs!!1))
            splitarg i = if numargs > i then (Util.splitBy ',' $ cmdargs!!i) else []
            ispageskip name = Util.isin name pagesskip
            ispageonly name = Util.isin name pagesonly
            ispageused name = not (ispageskip name) && (null pagesonly || Util.isin name pagesonly)
            readorcreate file = System.Directory.doesFileExist file >>=
                \isfile -> if isfile then readFile file else writeFile file "" >> return ""
        in do
            System.Directory.createDirectoryIfMissing True dirsite
            filestream_config <- readorcreate $ path "haxtatic.config"
            filestream_tmplmain <- readorcreate $ path "theme.tmpl.html"
            filestream_tmplblog <- readorcreate $ path "blog.tmpl.html"
            let
                dirout = path "build"
                dirpages = path "pages"
                dirposts = path "posts"
                dirstatic = path "static"
                sitename = (last $ Util.splitBy '/' $ Util.swapout '\\' '/' dirsite)
                txts = Config.txts cfgtmp where cfgtmp = lines filestream_config
                cfglines = lines $ Util.replacein filestream_config txts
                cfgexts = Config.exts cfglines blognamesall ; blognamesall = map Blogs.name blogs
                blogs = Config.blogs cfglines ; blognames = filter ispageused $ blognamesall
                daters = Config.daters cfglines monthname ; monthname m = monthnames!!(readInt m)
                monthnames = map (\mn -> Util.keyVal txts ("{{T:Hxm_"++(take 2 mn)++"}}") (drop 2 mn)) ["00","01January","02February","03March","04April","05May","06June","07July","08August","09September","10October","11November","12December"]
                splitdot = Util.splitBy '.'

                -- in pages dir, ignore . and .. and pick files named yyyy.mm.dd.*.* (hackily enough, we really pick *.*.*.*.*)
                filterPageFileNames filenames =
                    Control.Monad.filterM ispagefile filenames where
                        ispagefile filename = let srcpath = dirpages </> filename in
                            System.Directory.doesFileExist srcpath >>= \isafile ->
                                return $ isafile && ispageused (System.FilePath.takeDirectory filename)

                mapPageFileNames relpaths =
                    return pathsnames where
                        pathsnames = map totuple relpaths
                        totuple relpath = (fullpath,fname) where
                            fullpath = dirpages </> relpath
                            fname = dateparts ++ dirparts ++ nameparts
                            dateparts = if fnhasdate then take 3 fn else nowstrs
                            dirparts = if null reldir || reldir == "." then [] else Util.splitBy System.FilePath.pathSeparator reldir
                            nameparts = if fnhasdate then drop 3 fn else fn
                            reldir = takeDirectory relpath
                            fnhasdate = (length fn)>3 && (Util.within "0000" "9999" (head fn)) && (Util.within "01" "12" (fn!!1)) && (Util.within "01" "31" (fn!!2))
                            fn = splitdot $ System.FilePath.takeFileName relpath

                loadAllPosts pagepaths = let
                        ispostfile = (System.Directory.doesFileExist . (</>) dirposts)
                        per_postsfile filename = let srcpath = dirposts </> filename in
                            readFile srcpath >>= \rawsrc -> return $ Posts.loadPosts filename (applyprep rawsrc "")
                        sortposts = quickSort Posts.fn (>=) (<) -- only sort by date, newest to oldest
                        blogposts = Control.Monad.mapM topost $ filter isblogpage pagepaths where
                            topost (fullpath,fn) =
                                readFile (fullpath) >>= \rawsrc -> let
                                    h1 = Html.tagInner "h1" $ head rawlines ; rawlines = lines raw ; raw = applyprep rawsrc pcat
                                    p = if (length ps) > 0 then head ps else "" ; blog = blogbyname (Util.fnName fn)
                                    ps = filter (\l -> Util.is $ tagInner "p" l) rawlines
                                    ptext = Html.tagInnerPlain p
                                    pcat = (if Blogs.nameAsCat blog then Blogs.name else Blogs.title) blog
                                    (pname, ptitle, plink) = (fn, h1, Util.join "." (Util.drop3 fn))
                                    psubcat = (Util.keyVal daters (Blogs.df blog) (snd $ head daters)) fn
                                    in return $ [Posts.Post pname psubcat ptitle plink "" ptext pcat (Pages.processMarkupDumb pname (Blogs.name blog) (fn!!4) ptitle raw alltemplaters daters)]
                            isblogpage (_,pfn) = Util.isin (Util.fnName pfn) blognames
                        alltemplaters = concat $ map Pages.xTemplaters cfgexts
                        mergePosts allfileposts = blogposts >>= (return . concat . (++)allfileposts)
                        toAtoms allposts = map percat cats where
                            cats = Data.List.nub $ map Posts.cat allposts
                            percat cat = Posts.toAtom sitename cat (filter (\p->(cat==(Posts.cat p))) allposts) blogbyname
                    in System.Directory.doesDirectoryExist dirposts >>=
                        (\isdir ->if isdir then System.Directory.getDirectoryContents dirposts else return []) >>=
                            Control.Monad.filterM ispostfile >>= Control.Monad.mapM per_postsfile >>= mergePosts >>=
                                \allposts -> let
                                    atoms = toAtoms allposts ; pfns = map snd pagepaths
                                    peratom a = if (null txt) || (null afn) then return ()
                                        else writefile afn txt where (afn,txt) = a
                                    sortedposts = sortposts allposts
                                    in do
                                        Control.Monad.mapM peratom atoms
                                        writefile "sitemap.xml" (Pages.toSitemap sitename pfns blognames pagesnositemap)
                                        return $ map (\(fullpath,pfn) -> ((fullpath, pfn), sortedposts, alltemplaters)) pagepaths

                blogbyname bn = head $ filter (\b -> (Blogs.name b) == bn) blogs
                writefile fn c = putStr ("\t"++fn++" [ ") >> writeFile (dirout </> fn) c >> putStrLn "OK ]"

                -- read a page file and create a Pages.Ctx from its raw source
                per_pagesrcfile ((fullpath,pfn),allposts,alltemplaters) =
                    readFile (fullpath) >>= \rawsrc ->
                        return $ pagefromsrc allposts alltemplaters pfn (applyprep rawsrc "")

                -- given file-name info and raw page source, create a Pages.Ctx "ready for applyTemplate"
                pagefromsrc = Pages.newPageContext nowint daters cfgexts
                nowint = floor $ toRational $ Data.Time.Clock.utctDayTime nowtime
                nowstrs = Util.splitBy '-' $ take 10 (show nowtime)

                -- given list of Pages.Ctx (plus generating one more on-the-fly for the blog-index page),
                -- apply template and generate final output page file for each
                genpages pages =
                    Control.Monad.mapM_ per_page allpages where
                        allpages = if ((length bpages) < 1) then pages else (blogindexpages ++ pages)
                        per_page page = applyTemplate page >>= writefile (outfilename page)
                        bpages = filter isblog pages where isblog page = (Util.isin (Util.fnName $ Pages.fname page) blognames)
                        outfilename page = Util.join "." $ Util.drop3 $ Pages.fname page
                        blogindexpages = map blogindexpage blognames
                        blogindexpage bname =
                            pagefromsrc allposts alltemplaters [year,month,day,bname,"html"] (applyprep filestream_tmplblog bname) where
                                year = Util.fnYear bpfn ; month = Util.fnMonth bpfn ; day = Util.fnDay bpfn
                                (allposts,alltemplaters) = (Pages.allPosts fstpage , Pages.allXTemplaters fstpage)
                                fstpage = head pages ; bph = head bps ; bps = (filter (\bp -> bname == (Util.fnName $ Pages.fname bp)) bpages)
                                bpfn = if (length bps) > 0 then Pages.fname bph else nowstrs

                -- given a Pages.Ctx (and list of all Pages.Ctx for blog posts), render final output-page content
                applyTemplate page =
                    return $ Pages.postProcessMarkup page $ unlines $ map per_line (lines (applyprep replbod "")) where
                        replbod = Data.List.Utils.replace "{{P:Body}}" (Pages.body page) filestream_tmplmain
                        per_line ln = concat $ Pages.processMarkupLn page ln
                applyprep s = Blogs.tmplMarkupSrc blogs $ Util.replacein s txts

                in -- now let's go!
                    Control.Monad.mapM (System.Directory.createDirectoryIfMissing False) [dirout, dirpages, dirposts, dirstatic]
                    >> putStrLn ("\n=== HAXTATIC ===\nCopying everything inside '"++dirstatic++"'\nover into '"++dirout++"'..")
                    >> System.Directory.getDirectoryContents dirstatic
                    >>= Util.copyAll dirstatic dirout (not skipstaticfolders)
                    >> putStrLn ("\t[ OK ]\nLoading from '"++dirpages++"' & '"++dirposts++"'\nand generating in '"++dirout++"'..")
                    >> Util.getAllFiles dirpages ""
                    >>= filterPageFileNames
                    >>= mapPageFileNames
                    >>= loadAllPosts
                    >>= Control.Monad.mapM per_pagesrcfile
                    >>= genpages
