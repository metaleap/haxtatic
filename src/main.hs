module Main where

import qualified Blogs
import qualified Config
import qualified Html
import qualified MainDefaults
import qualified Pages
import qualified Posts
import qualified Util

import qualified Control.Monad
import qualified Data.List
import qualified Data.List.Utils
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO


helpmsg = "\n\n\n==== HAXTATIC ====\nNo project directory path given.\n  For existing project: specify its directory.\n\
    \  For a new project structure: specify its intended directory."


main = do
    -- grab some definitely-needed inputs right now or fail right here
    starttime <- Data.Time.Clock.getCurrentTime
    cmdargs <- System.Environment.getArgs
    let numargs = length cmdargs in if numargs < 1 then putStrLn helpmsg else
        let dirsite = head cmdargs ; path = (</>) dirsite
            sitename = (last $ Util.splitBy '/' $ Util.swapout '\\' '/' dirsite)
            pagesnositemap = splitarg 2 ; pagesskip = splitarg 3 ; pagesonly = splitarg 4
            skipstaticfolders = numargs>1 && ("True"==(cmdargs!!1))
            splitarg i = if numargs > i then (Util.splitBy ',' $ cmdargs!!i) else []
            ispageskip name = Util.isin name pagesskip
            ispageonly name = Util.isin name pagesonly
            ispageused name = not (ispageskip name) && (null pagesonly || Util.isin name pagesonly)
            readorcreate defsrc fn = let fp = path fn in System.Directory.doesFileExist fp >>=
                \isfile -> if isfile then readFile fp else writefilein dirsite fn defsrc >> return defsrc
            writefilein dir fn c =
                let fp = dir</>fn ; l = 1+(length dirsite) in
                    putStr ("\t>> "++(drop l fp)++"  [ ") >> System.IO.hFlush System.IO.stdout >> writeFile fp c >> putStrLn "OK ]"
        in do
            putStrLn "\n\n\n==== HAXTATIC ====\n\n1. Reading essential project files or (re)creating them.."
            System.Directory.createDirectoryIfMissing True dirsite
            dircur <- System.Directory.getCurrentDirectory
            filestream_config <- readorcreate (MainDefaults.haxConf sitename) "haxtatic.config"
            filestream_tmplmain <- readorcreate MainDefaults.themeHtml "theme.tmpl.html"
            filestream_tmplblog <- readorcreate MainDefaults.blogHtml "blog.tmpl.html"
            let
                writefile2outdir = writefilein dirout
                dirout = path "build"
                dirpages = path "pages"
                dirposts = path "posts"
                dirstatic = path "static"
                txts = Config.txts cfgtmp where cfgtmp = lines filestream_config
                cfglines = lines $ Util.replacein filestream_config txts
                cfgexts = Config.exts cfglines blognamesall ; blognamesall = map Blogs.name blogs
                blogs = Config.blogs cfglines ; blognames = filter ispageused $ blognamesall
                daters = Config.daters cfglines monthname ; monthname m = monthnames!!(Util.readInt m)
                monthnames = map (\mn -> Util.keyVal txts ("{T{HxM_"++(take 2 mn)++"}}") (drop 2 mn)) ["00","01January","02February","03March","04April","05May","06June","07July","08August","09September","10October","11November","12December"]
                splitdot = Util.splitBy '.'

                createDefaultPageIfEmpty filenames =
                    if not $ null filenames then return filenames
                        else let fn = "index.html" ; fp = dirpages </> fn in
                            do writefilein dirpages fn $ MainDefaults.indexHtml dircur sitename dirsite dirpages fp (path "theme.tmpl.html") (dirout </> fn)
                            >> return [fn]

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
                            reldir = System.FilePath.takeDirectory relpath
                            fnhasdate = (length fn)>3 && (Util.within "0000" "9999" (head fn)) && (Util.within "01" "12" (fn!!1)) && (Util.within "01" "31" (fn!!2))
                            fn = splitdot $ System.FilePath.takeFileName relpath

                createSitemapXml pagepaths = do
                    writefile2outdir "sitemap.xml" (Pages.toSitemap sitename (map snd pagepaths) blognames pagesnositemap)
                    return pagepaths

                loadAllPosts pagepaths = let
                        ispostfile = (System.Directory.doesFileExist . (</>) dirposts)
                        per_postsfile filename = let srcpath = dirposts </> filename in
                            readFile srcpath >>= \rawsrc -> return $ Posts.loadPosts filename (applyprep rawsrc "")
                        sortposts = Util.quickSort Posts.fn (>=) (<) -- only sort by date, newest to oldest
                        blogposts = Control.Monad.mapM topost $ filter isblogpage pagepaths where
                            topost (fullpath,fn) =
                                readFile (fullpath) >>= \rawsrc -> let
                                    h1 = Html.tagInner "h1" $ head rawlines ; rawlines = lines raw ; raw = applyprep rawsrc pcat
                                    p = if (length ps) > 0 then head ps else "" ; blog = blogbyname (Util.fnName fn)
                                    ps = filter (\l -> Util.is $ Html.tagInner "p" l) rawlines
                                    ptext = Html.tagInnerPlain p
                                    pcat = (if Blogs.nameAsCat blog then Blogs.name else Blogs.title) blog
                                    (pname, ptitle, plink) = (fn, h1, Util.join "." (Util.drop3 fn))
                                    psubcat = (Util.keyVal daters (Blogs.df blog) (snd $ head daters)) fn
                                    in return $ [Posts.Post pname psubcat ptitle plink "" ptext pcat (Pages.processMarkupDumbly4Feeds pname (Blogs.name blog) (fn!!4) ptitle raw alltemplaters daters)]
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
                                    atoms = toAtoms allposts
                                    peratom a = if (null txt) || (null afn) then return ()
                                        else writefile2outdir afn txt where (afn,txt) = a
                                    sortedposts = sortposts allposts
                                    in do
                                        Control.Monad.mapM peratom atoms
                                        return $ map (\(fullpath,pfn) -> ((fullpath, pfn), sortedposts, alltemplaters)) pagepaths

                blogbyname bn = head $ filter (\b -> (Blogs.name b) == bn) blogs

                -- read a page file and create a Pages.Ctx from its raw source
                per_pagesrcfile ((fullpath,pfn),allposts,alltemplaters) =
                    readFile (fullpath) >>= \rawsrc ->
                        return $ pagefromsrc allposts alltemplaters pfn (applyprep rawsrc "")

                -- given file-name info and raw page source, create a Pages.Ctx "ready for applyTemplate"
                pagefromsrc = Pages.newPageContext nowint daters cfgexts
                nowint = floor $ toRational $ Data.Time.Clock.utctDayTime starttime
                nowstrs = Util.splitBy '-' $ take 10 (show starttime)

                -- given list of Pages.Ctx (plus generating one more on-the-fly for the blog-index page),
                -- apply template and generate final output page file for each
                genpages pages =
                    Control.Monad.mapM_ per_page allpages where
                        allpages = if ((length bpages) < 1) then pages else (blogindexpages++pages)
                        per_page page =
                            applyTemplate page >>= return . (clearpvars allpages) >>= writefile2outdir (outfilename page)
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
                clearpvars allpages outraw =
                    let pvarnames = Data.List.nub $ concat $ map (map (\(k,_) -> k)) $ map Pages.pageVars allpages
                    in (Util.replacein outraw (map (\k -> (k,"")) pvarnames))

                in -- now let's go!
                    Control.Monad.mapM (System.Directory.createDirectoryIfMissing False)
                            [dirout, dirpages, dirposts, dirstatic]
                    >> putStrLn ("2. Copying what's inside '"++dirstatic++"'\n   over into '"++dirout++"'..")
                    >> System.Directory.getDirectoryContents dirstatic
                        >>= Util.copyAll dirstatic dirout (not skipstaticfolders)
                    >> putStrLn ("3. Scanning '"++dirpages++"'..")
                    >> Util.getAllFiles dirpages ""
                        >>= createDefaultPageIfEmpty
                        >>= filterPageFileNames
                        >>= mapPageFileNames
                        >>= createSitemapXml
                        >>= Util.via (putStrLn $ "4. Loading from '"++dirposts++"'..")
                        >>= loadAllPosts
                        >>= Util.via (putStrLn $ "5. Loading from '"++dirpages++"'..")
                        >>= Control.Monad.mapM per_pagesrcfile
                        >>= Util.via (putStrLn $ "6. Generating outputs in '"++dirout++"':")
                        >>= genpages
                        >>  Data.Time.Clock.getCurrentTime
                            >>= \now -> let timetaken = show $ Util.since starttime now in
                                    putStrLn $ "\n\nThis all took "++timetaken++" --- bye now!\n\n\n"
