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
    in ( inner , attrs~|fst~.is )



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
    tagname = tag-:name
    tagatts = tag-:attribs
    innercontent = Util.lookup "" "" tagatts
    tchildren = tag-:subTags
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
    defval -|= (finder htmlsrc)@?0

findInnerContentOfTags tagname htmlsrc =
    let lookfor = ['<':(tagname++">") , '<':(tagname++" ")] -- remember all need same length for single splitUp call!!!  -- ,'<':(tagname++"\t"),'<':(tagname++"\n")] ..?! let's not overdo this
        chunks = Util.splitUp id lookfor ("</"++tagname++">") htmlsrc
        foreach (_,"") = Nothing
        foreach ("",_) = Nothing
        foreach (inner'' , tagbegin) =
            let inner' = if tagbegin == lookfor@!0 then inner'' else snd$ Util.splitOn1st '>' inner''
                inner = Util.trim inner'
            in null inner |? Nothing |! Just inner
    in chunks>~foreach ~> Util.unMaybes

findValuesOfVoidTags1stAttr tagname attrname htmlsrc =
    let chunks = Util.splitUp id ['<':(tagname++" "++attrname++"=\"")] "/>" htmlsrc
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
