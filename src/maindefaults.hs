module MainDefaults where

import Data.Char
import System.FilePath


haxConf sitename = "T:SiteTitle: "++(map Data.Char.toUpper sitename)++"-Site"

themeHtml = "<!DOCTYPE html><html lang=\"en\"><head>\n\
    \    <title>{{P:Title}} - {{T:SiteTitle}}</title>\n\
    \    <style type=\"text/css\"> h3 { text-align: center; color: #f3f4f2; background: #636462; padding: 0.66em; letter-spacing: 0.33em; font-weight: normal; font-size: 1.44em; } div { text-align: right; font-style: italic; } body { font-family: sans-serif; background: #f3f4f2; line-height: 1.44em; } code { background: #c3c4c2; } </style>\n\
    \    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n\
    \</head><body>\n\
    \    <h3>{{T:SiteTitle}}</h3>\n\
    \    <!-- begins generated content -->\n\
    \{{P:Body}}\n\
    \    <!-- end of generated content -->\n\
    \    <hr/><div>Generated with <a href=\"http://github.com/HaXtatic\">{{P:Var:demo_hax}}</a> on {{P:Date}}</div>\n\
    \</body></html>"

blogHtml = "<h1>{{B:Title:}}</h1>\n<p>{{B:Desc:}}</p>"

indexHtml dircur sitename dirsite dirpages pathpage pathtmpl pathfinal =
    let l = 1+(length dirsite) ; x s = "{{P:Var:demo_dirpath}}<b>"++(drop (l) s)++"</b>" in
        "<h1>Greetings..</h1>\n\
            \{{P:Var:demo_hax:<b>HaXtatic</b>}}\n\
            \{{P:Var:demo_dirpath:"++dirsite++[System.FilePath.pathSeparator]++"}}\n\
            \<p>Looks like for now I&apos;m the home page of your static site <code>"++sitename++"</code>! How did this come about?</p>\n\
            \<p>When you ran {{P:Var:demo_hax}} from <code>"++dircur++"</code>, specifying project-directory <code><b>{{P:Var:demo_dirpath}}</b></code>:</p>\n\
            \<ul>\n\
            \    <li>I was generated at <code>"++(x pathfinal)++"</code> by</li>\n\
            \    <li>..applying the <code>"++(x pathtmpl)++"</code> template (ready for your tinkering)</li>\n\
            \    <li>..to my &apos;<i>content source page</i>&apos; stored at <code>"++(x pathpage)++"</code> (dito)</li>\n\
            \    <li>..which in turn just-beforehand {{P:Var:demo_hax}} pre-created for you (but only because <code>"++(x dirpages)++[System.FilePath.pathSeparator]++"</code> was entirely devoid of files: otherwise it won&apos;t meddle in there).</li>\n\
            \</ul>"
