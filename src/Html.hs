{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
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
    deriving Eq



attrEscapeVals [] =
    []
attrEscapeVals ((thisname,thisval):rest) =
    ( thisname , (null thisname) |? thisval |! escape thisval ):(attrEscapeVals rest)



attrClearInner attrs =
    let inner = Util.lookup "" "" attrs
    in ( inner , Util.noNilFsts attrs )



emit::
    Tag->
    String
emit tag =
    nooutertag |? outinner |!
        (outopen++outatts++outinner++outclose)
    where
    outopen = "<"++tagname
    outatts = concat$ (Util.mergeDuplFsts (==) (Util.join " ") tagatts)>~foreach where
        foreach (n,v)   |(null n || null v)= ""
                        |(otherwise)= " " ++ n ++ "=\"" ++ v ++ "\""
    outinner = ifselfclosing ++ (concat$ tchildren>~emit) ++ innercontent where
        ifselfclosing = nooutertag |? "" |!
                        noinneroutput |? "/>" |! ">"
    outclose = noinneroutput |? "" |! ("</"++tagname++">")

    nooutertag = null tagname
    tagname = tag-:name
    tagatts = tag-:attribs
    innercontent = Util.lookup "" "" tagatts
    tchildren = tag-:subTags
    noinneroutput = null tchildren && null innercontent



escape =
    --  standard Util.replaceSubs was still too slow given that we know *exactly*
    --  which 5 possible *Chars* (not even Strings) to look for AND how to replace them
    esc where
    esc str =
        let (idx , rest , replacewith) = next 0 str
        in if idx<0 then str else
            (take idx str) ++ replacewith ++ (esc rest)
    next _ [] =
        (minBound::Int , [] , "")
    next count ('\'':rest) =
        (count , rest , "&apos;")
    next count ('\"':rest) =
        (count , rest , "&quot;")
    next count ('&':rest) =
        (count , rest , "&amp;")
    next count ('<':rest) =
        (count , rest , "&lt;")
    next count ('>':rest) =
        (count , rest , "&gt;")
    next count (_:rest) =
        next (count + 1) rest

escapeSpace4Href = Util.replaceWith (' ' , "%20")


find1st finder defval htmlsrc =
    defval -|= (finder htmlsrc)@?0

findInnerContentOfTags tagname htmlsrc =
    let lookfor = ['<':(tagname++">") , '<':(tagname++" ")] -- remember ALL need same length for single splitUp call!!!  -- ,'<':(tagname++"\t"),'<':(tagname++"\n")] ..?! let's not overdo this
        chunks = Util.splitUp id lookfor ("</"++tagname++">") htmlsrc
        foreach (_,"") = Nothing
        foreach ("",_) = Nothing
        foreach (inner'' , tagbegin) =
            let inner' = if (tagbegin == lookfor@!0) then inner'' else snd$ Util.splitOn1st_ '>' inner''
                inner = Util.trim inner'
            in null inner |? Nothing |! Just inner
    in chunks>=~foreach

findValuesOfVoidTags1stAttr tagname attrname htmlsrc =
    let chunks = Util.splitUp id ['<':(tagname++" "++attrname++"=\"")] "/>" htmlsrc
        foreach (_,"") = Nothing
        foreach (inner',_) =
            let (inner,_) = Util.splitOn1st_ '\"' inner'
            in null inner |? Nothing |! Just inner
    in chunks>=~foreach



joinUri relpath "" =
    Files.sanitizeUriRelPathForJoin relpath
joinUri "" relpath' =
    Files.sanitizeUriRelPathForJoin relpath'
joinUri relpath relpath' =
    (Files.sanitizeUriRelPathForJoin relpath) ++ ('/':Files.sanitizeUriRelPathForJoin relpath')



out::
    String-> Util.StringPairs-> [Tag]->
    String
out tname tatts tchildren =
    emit (T tname tatts tchildren)



rootPathToRel currelpath =
    if null walktoroot then id else ((last walktoroot) ++)
    where
    walktoroot = take (Util.count '/' (Files.sanitizeUriRelPathForJoin currelpath)) infinity
    infinity = iterate (++ "../") "../"



stripMarkup stripentities substchar markup =
    stripmarkup (False , False) markup
    where
    stripmarkup _ [] = []
    stripmarkup (intagis , inentis) (curchar:rest) =
        nextchar:(stripmarkup (intagnext , inentnext) rest)
        where
        nextchar = (intagnow || inentnow) |? substchar |! curchar

        intagnow = intagis || istagopen
        intagnext = intagnow && not istagclose
        istagopen = curchar=='<'
        istagclose = curchar=='>'

        inentnow = stripentities && (inentis || isentopen)
        inentnext = inentnow && not isentclose
        isentopen = curchar=='&'
        isentclose = curchar==';'
