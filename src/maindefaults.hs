{-# OPTIONS_GHC -Wall #-}
module MainDefaults where


import qualified Data.Char

import qualified System.FilePath


haxConf::
    String->
    String
haxConf sitename = "T:SiteTitle: "++(map Data.Char.toUpper sitename)++"-Site"

themeHtml:: String
themeHtml = "<!DOCTYPE html><html lang=\"en\"><head>\n\
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
    \    <div><!-- begins {P{FileName}} content generated from {P{OrigPath}} -->\n\n\n\
    \{P{Body}}\n\n\
    \    </div><!-- end of generated content -->\n\
    \    <hr/><small>Generated with <a href=\"http://github.com/HaXtatic\">{P{%demo_hax}}</a> on {P{Date}}</small>\n\
    \</body></html>"

blogHtml:: String
blogHtml = "<h1>{B{Title:_}}</h1>\n\
    \<p>{B{Desc:_}}</p>\n\
    \<p>\n\
    \For a neat overview listing of all your <code>{B{Name:_}}.*.html</code> (and/or <code>{B{Name:_}}/*.html</code>)\n\
    \articles within your <code>pages</code> folder on here, check out the <code>{X{Listings}}</code> tag type in the HaXtatic docs.\n\
    \</p>"

indexHtml::
    String-> String-> String-> String-> String-> String-> String->
    String
indexHtml dircur sitename dirsite dirpages pathpage pathtmpl pathfinal =
    let l = 1+(length dirsite) ; x s = "{P{%demo_dirpath}}<b>"++(drop (l) s)++"</b>" in
        "<h1>Greetings..</h1>\n\
            \{P{%demo_hax:<b>HaXtatic</b>}}\n\
            \{P{%demo_dirpath:"++dirsite++[System.FilePath.pathSeparator]++"}}\n\
            \<p>Looks like for now I&apos;m the home page of your static site <code>"++sitename++"</code>! How did this come about?</p>\n\
            \<p>When you ran {P{%demo_hax}} from <code>"++dircur++"</code>, specifying project-directory <code><b>{P{%demo_dirpath}}</b></code>:</p>\n\
            \<ul>\n\
            \    <li>I was generated at <code>"++(x pathfinal)++"</code> by</li>\n\
            \    <li>..applying the <code>"++(x pathtmpl)++"</code> template (ready for your tinkering)</li>\n\
            \    <li>..to my &apos;<i>content source page</i>&apos; stored at <code>"++(x pathpage)++"</code> (dito)</li>\n\
            \    <li>..which in turn {P{%demo_hax}} pre-created for you just-beforehand (but only because <code>"++(x dirpages)++[System.FilePath.pathSeparator]++"</code> was totally devoid of any files: otherwise it won&apos;t meddle in there as a rule).</li>\n\
            \</ul>"
