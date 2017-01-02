{-# OPTIONS_GHC -Wall #-}
module Html where

import qualified Util
import Util ( noNull , (~:) , (>~) , (#) , (~|) )



data Tag =
    T {
        name :: String,
        attribs :: Util.StringPairs,
        subTags :: [Tag]
    }
    deriving (Eq, Read)



basicEscapes ::
    Util.StringPairs
basicEscapes = [("\"","&quot;"),("'","&apos;"),(">","&gt;"),("<","&lt;"),("&","&amp;")]


emit::
    Tag->
    String
emit tag =
    if nooutertag then outinner else
        outopen++outatts++outinner++outclose
    where
    outopen = "<"++tagname
    outatts = concat$ tagatts>~foreach where
        foreach (n,v)   |(null n || null v)= ""
                        |(otherwise)= " " ++n++ "=\"" ++v++ "\""
    outinner = ifselfclosing++(concat$ tchildren>~emit)++innercontent where
        ifselfclosing = if nooutertag then "" else
            if noinneroutput then "/>\n" else
                ">" ++ if noNull innercontent then "" else "\n"
    outclose = if noinneroutput then ""
        else "</" ++tagname++ ">\n"

    nooutertag = null tagname
    tagname = tag~:name
    tagatts = tag~:attribs
    innercontent = if noNull tagatts && null att0n then att0v else "" where
        (att0n,att0v) = tagatts#0
    tchildren = tag~:subTags
    noinneroutput = null tchildren && null innercontent



innerContentsNoAtts tagname htmlsrc =
    let chunks = Util.splitUp ["<"++tagname++">"] ("</"++tagname++">") htmlsrc
        foreach (_,"") = ""
        foreach (inner,_) = inner
    in chunks>~foreach ~|noNull



out::
    String-> Util.StringPairs-> [Tag]->
    String
out tname tatts tchildren =
    emit$ T tname tatts tchildren



stripTags::
    String->
    String
stripTags markup =
    striptags False markup
    where
    striptags _ [] = []
    striptags intagwas (c:rest) =
        char:(striptags intagnext rest) where
            char = if intagnow then ' ' else c
            intagnow = intagwas || istagopen || istagclose
            intagnext = intagnow && not istagclose
            istagopen = c=='<'
            istagclose = c=='>'
