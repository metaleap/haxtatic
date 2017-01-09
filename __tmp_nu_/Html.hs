{-# OPTIONS_GHC -Wall #-}
module Html where

import Base
import qualified Files
import qualified Util



data Tag =
    T {
        name :: String,
        attribs :: Util.StringPairs,
        subTags :: [Tag]
    }
    deriving (Eq)



attrEscapeVals [] =
    []
attrEscapeVals ((thisname,thisval):rest) =
    ( thisname , (null thisname) |? thisval |! escape thisval ):(attrEscapeVals rest)



attrClearInner attribs =
    let inner = Util.lookup "" "" attribs
    in ( inner , attribs~|fst~.is )



emit::
    Tag->
    String
emit tag =
    nooutertag |? outinner |!
        outopen++outatts++outinner++outclose
    where
    outopen = "<"++tagname
    outatts = concat$ (Util.uniqueFst tagatts)>~foreach where
        foreach (n,v)   |(null n || null v)= ""
                        |(otherwise)= " " ++ n ++ "=\"" ++ v ++ "\""
    outinner = ifselfclosing ++ (concat$ tchildren>~emit) ++ innercontent where
        ifselfclosing = nooutertag |? "" |!
                            noinneroutput |? "/>" |! ">"
    outclose = noinneroutput |? "" |! "</"++tagname++">"

    nooutertag = null tagname
    tagname = tag.:name
    tagatts = tag.:attribs
    innercontent = Util.lookup "" "" tagatts
    tchildren = tag.:subTags
    noinneroutput = null tchildren && null innercontent



escape =
    --  standard Util.replaceSubs was still too slow given that we know *exactly*
    --  which 5 *Chars* (not even Strings) to look for AND how to replace them
    esc where
    esc str =
        let (idx , rest , repl) = nextidx 0 str
        in if idx<0 then str else
            (take idx str) ++ repl ++ (esc rest)
    nextidx _ [] =
        (minBound::Int , [] , "")
    nextidx count ('\'':rest) =
        (count , rest , "&apos;")
    nextidx count ('\"':rest) =
        (count , rest , "&quot;")
    nextidx count ('&':rest) =
        (count , rest , "&amp;")
    nextidx count ('<':rest) =
        (count , rest , "&lt;")
    nextidx count ('>':rest) =
        (count , rest , "&gt;")
    nextidx count (_:rest) =
        nextidx (count + 1) rest



find1st finder defval htmlsrc =
    Util.atOr (finder htmlsrc) 0 defval

findInnerContentsNoAtts tagname htmlsrc =
    let chunks = Util.splitUp id ['<':(tagname++">")] ("</"++tagname++">") htmlsrc
        foreach (_,"") = Nothing
        foreach ("",_) = Nothing
        foreach (inner',_) =
            let inner = Util.trim inner'
            in null inner |? Nothing |! Just inner
    in chunks>~foreach ~> Util.unMaybes

findValuesOfSingleAtt tagname attrname htmlsrc =
    let chunks = Util.splitUp id ['<':(tagname++" "++attrname++"=\"")] "\" />" htmlsrc
        foreach (_,"") = Nothing
        foreach (inner',_) =
            let (inner,_) = Util.splitOn1st '\"' inner'
            in null inner |? Nothing |! Just inner
    in chunks>~foreach ~> Util.unMaybes



joinUri relpath relpath' =
    (Files.sanitizeUriRelPathForJoin relpath) ++ ('/':Files.sanitizeUriRelPathForJoin relpath')



out::
    String-> Util.StringPairs-> [Tag]->
    String
out tname tatts tchildren =
    emit (T tname tatts tchildren)



stripMarkup::
    Char->
    String->
    String
stripMarkup substchar markup =
    stripmarkup False markup
    where
    stripmarkup _ [] = []
    stripmarkup intagwas (curchar:rest) =
        nextchar:(stripmarkup intagnext rest)
        where
        nextchar = intagnow |? substchar |! curchar
        intagnow = intagwas || istagopen || istagclose
        intagnext = intagnow && not istagclose
        istagopen = curchar=='<'
        istagclose = curchar=='>'
