{-# OPTIONS_GHC -Wall #-}

module Defaults where

import qualified Files
import Util ( (>~) , (~:) )

import qualified Data.Char
import qualified Data.Time.Clock
import qualified Data.Time.Calendar
import qualified Data.Time.Format
import qualified System.FilePath
import System.FilePath ( (</>) )


--  the basic default input files
data CoreFiles = CoreFiles {
    projectDefault :: Files.File,
    projectOverwrites :: Files.File,
    htmlTemplateMain :: Files.File,
    htmlTemplateBlok :: Files.File
}



loadOrCreate ctxmain projname projfilename custfilename =
    let projfilecontent = _proj projname
        setupname = setupName projfilename
    in Files.readOrDefault True ctxmain projfilename fileName_Proj projfilecontent
    >>= \ projfile
    -> Files.readOrDefault True ctxmain custfilename "" ""
    >>= \ custfile
    -> Files.readOrDefault True ctxmain (setupname++"-main.haxtmpl.html") (fileName_Pref "-main.haxtmpl.html") _tmpl_html_main
    >>= \ tmplmainfile
    -> Files.readOrDefault True ctxmain (setupname++"-blok.haxtmpl.html") (fileName_Pref "-blok.haxtmpl.html") _tmpl_html_blok
    >>= \ tmplblokfile
    -> return (CoreFiles projfile custfile tmplmainfile tmplblokfile)


rewriteTemplates corefiles tmplrewriter =
    let cfgmodtime = corefiles~:projectDefault~:Files.modTime~:max$
                        corefiles~:projectOverwrites~:Files.modTime
        tmplmodtime = cfgmodtime~:max$
                        corefiles~:htmlTemplateMain~:Files.modTime
        rewrite modtime rw file
            = Files.fullFrom file modtime (rw $file~:Files.content)
    in CoreFiles {
        projectDefault = rewrite cfgmodtime id $corefiles~:projectDefault,
        projectOverwrites = rewrite cfgmodtime id $corefiles~:projectOverwrites,
        htmlTemplateMain = rewrite tmplmodtime tmplrewriter $corefiles~:htmlTemplateMain,
        htmlTemplateBlok = rewrite tmplmodtime tmplrewriter $corefiles~:htmlTemplateBlok
    }


writeDefaultIndexHtml ctxmain projname dirpagesrel dirbuild htmltemplatemain =
    let
        dircur = ctxmain~:Files.curDir
        dirproj = ctxmain~:Files.dirPath
        dirpages = dirproj </> dirpagesrel
        outfilepath = dirpages </> fileName_IndexHtml
        outfilerel = dirpagesrel </> fileName_IndexHtml
        pathtmpl = htmltemplatemain~:Files.path
        pathfinal = dirbuild </> fileName_IndexHtml
        outfilecontent = return$ _index_html
                            dircur projname dirproj dirpages outfilepath pathtmpl pathfinal
        outfile = Files.FileInfo outfilepath (ctxmain~:Files.nowTime)
    in
        Files.writeTo outfilepath outfilerel outfilecontent
        >> return (outfile , outfilerel , pathfinal)


setupName = System.FilePath.takeBaseName



blokIndexTmpPathPrefix = "|:B:|"
dateTimeFormat = Data.Time.Format.iso8601DateFormat Nothing
dateTime0 = Data.Time.Clock.UTCTime {   Data.Time.Clock.utctDay = Data.Time.Calendar.ModifiedJulianDay { Data.Time.Calendar.toModifiedJulianDay = 0 },
                                        Data.Time.Clock.utctDayTime = 0 }
dir_Out = "build"
dir_Deploy =""
dir_Static = "static"
dir_Pages = "pages"
dir_Posts = "posts"
dir_PostAtoms = dir_PostAtoms_None
dir_PostAtoms_None = ":"
fileName_IndexHtml = "index.html"
fileName_Proj = fileName_Pref ".haxproj"
fileName_Pref = ("default"++)


_proj name =
    "T::SiteTitle: "++(name >~ Data.Char.toUpper)++"-Site\n"


_index_html dircur sitename dirproj dirpages pathpage pathtmpl pathfinal =
    let l = 1+(length dirproj) ; x s = "{P{%demo_dirpath}}<b>"++(drop l s)++"</b>" in
        "<h1>Greetings..</h1>\n\
            \{P{%demo_hax:<b>HaXtatic</b>}}\n\
            \{P{%demo_dirpath:"++dirproj++[System.FilePath.pathSeparator]++"}}\n\
            \<p>Looks like for now I&apos;m the home page of your static site <code>"++sitename++"</code>! How did this come about?</p>\n\
            \<p>When you ran {P{%demo_hax}} from <code>"++dircur++"</code>, specifying project-directory <code><b>{P{%demo_dirpath}}</b></code>:</p>\n\
            \<ul>\n\
            \    <li>I was generated at <code>"++(x pathfinal)++"</code> by</li>\n\
            \    <li>..applying the <code>"++(x pathtmpl)++"</code> template (ready for your tinkering)</li>\n\
            \    <li>..to my &apos;<i>content source page</i>&apos; stored at <code>"++(x pathpage)++"</code> (dito)</li>\n\
            \    <li>..which in turn {P{%demo_hax}} pre-created for you just-beforehand &mdash; <b>but <i>only</i></b> because <code>"++(x dirpages)++[System.FilePath.pathSeparator]++"</code> was totally devoid of any files: otherwise it won&apos;t ever write to your content source directories.</li>\n\
            \</ul>"


_tmpl_html_blok =
    "<h1>{B{Title:_}}</h1>\n\
    \<p>{B{Desc:_}}</p>\n\
    \<p>\n\
    \For a neat overview listing of all your <code>pages/{B{Name:_}}.*.html</code> (and/or <code>pages/{B{Name:_}}/*.html</code>)\n\
    \articles on this page, check out the <code>{X{Listings}}</code> tag type in the HaXtatic docs.\n\
    \</p>"


_tmpl_html_main =
    "<!DOCTYPE html><html lang=\"en\"><head>\n\
    \    <meta content=\"text/html;charset=utf-8\" http-equiv=\"Content-Type\" />\n\
    \    <title>{P{Title}} - {T{SiteTitle}}</title><style type=\"text/css\">\n\
    \        h3 { text-align: center; color: CaptionText; background: ActiveCaption; padding: 0.66em; letter-spacing: 0.33em; font-size: 1.44em; border-radius: 1em; border: 0.123em dotted Background; }\n\
    \        small { display: block; background-color: InfoBackground; color: InfoText; text-align: right; font-style: italic; padding: 0.3em; margin: 0.3em; }\n\
    \        body { font-family: sans-serif; background: ButtonFace; color: ButtonText; line-height: 1.44em; }\n\
    \        code { background-color: Highlight; color: HighlightText; }\n\
    \        h1 { color: Background; }\n\
    \        div { padding: 1.23em; margin: 1.23em; background-color: Window; color: WindowText; border: 0.123em ButtonShadow inset; }\n\
    \    </style><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n\
    \</head><body>\n\
    \    <h3>{T{SiteTitle}}</h3>\n\
    \    <div><!-- begin {P{FileName}} content generated from {P{OrigPath}} -->\n\n\n\
    \{P{Body}}\n\n\
    \    </div><!-- end of generated content -->\n\
    \    <hr/><small>Generated with <a href=\"http://github.com/HaXtatic\">{P{%demo_hax}}</a> on {P{Date}}</small>\n\
    \</body></html>"
