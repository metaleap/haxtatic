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
            -- concat [take idx str , repl , esc rest]  --  whyyy is concat slower than multiple ++ .....
    nextidx _ [] =
        (minBound::Int , [] , "")
    nextidx count list@(this:rest)
        |(this=='\'')= (count , rest , "&apos;")
        |(this=='\"')= (count , rest , "&quot;")
        |(this=='<')= (count , rest , "&lt;")
        |(this=='>')= (count , rest , "&gt;")
        |(this=='&')= (count , rest , "&amp;")
        |(otherwise)= nextidx (count + 1) rest



innerContentsNoAtts oninner tagname htmlsrc =
    let chunks = Util.splitUp id ["<"++tagname++">"] ("</"++tagname++">") htmlsrc
        foreach (inner,tbegin) =
            if null tbegin then Nothing
                else Just inner
    in chunks>~foreach ~> Util.unMaybes >~ oninner



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
