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
    deriving (Eq, Read)



basicEscapes ::
    Util.StringPairs
basicEscapes = [ "\""=:"&quot;" , "'"=:"&apos;" , ">"=:"&gt;" , "<"=:"&lt;" , "&"=:"&amp;" ]



attrEscapeVals [] =
    []
attrEscapeVals ((thisname,thisval):rest) =
    ( thisname , (null thisname) |? thisval |! escape [] thisval ):(attrEscapeVals rest)



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



escape moreescapes =
    Util.replaceAll (basicEscapes++moreescapes)



innerContentsNoAtts tagname htmlsrc =
    let chunks = Util.splitUp ["<"++tagname++">"] ("</"++tagname++">") htmlsrc
        foreach (_,"") = ""
        foreach (inner,_) = inner
    in chunks>~foreach ~|is



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
