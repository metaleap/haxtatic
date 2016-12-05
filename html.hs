module Html where

import Util


data Tag = T { tagname :: String, tagatts :: Util.KeyVals, tagchildren :: [Tag] } deriving (Eq)


emit tag = if notag then inner else open++atts++inner++close where
    open = "<"++tname
    atts = concat $ map emitatt $ tatts where
        emitatt (n,v) = when (n=="" || v=="") "" (" "++n++"='"++v++"'")
    inner = (if notag then "" else when noinner "/>" ">")++(concat $ map emit tchildren)++tinner
    close = when noinner "" ("</"++tname++">")

    notag = null tname
    tname = tagname tag
    tatts = tagatts tag
    tinner = when (tatts/=[] && att0n=="") att0v "" where att0n = fst att0 ; att0v = snd att0 ; att0 = head tatts
    tchildren = tagchildren tag
    noinner = tchildren==[] && tinner==""


noTag tag = null $ tagname tag


out tname tatts tchildren = emit $ T tname tatts tchildren


-- HACKY! if line starts with <name> and ends with </name>, get inner text, else empty
t name = "<"++name++">"
t' name = "</"++name++">"
tagInner name line =
    let ll = length line ; lt = length name ; ls = lt+2 ; le = lt+3
        in if ll>(ls+le) && ((t name)==(take ls line)) && ((t' name)==(drop (ll-le) line))
            then drop ls (take (ll-le) line) else ""


tagInnerPlain markup = striptags False markup where
    striptags _ [] = []
    striptags intagwas (c:rest) = char:(striptags intagnext rest) where
        char = if intagnow then ' ' else c; intagnow = intagwas || istagopen || istagclose ; intagnext = intagnow && not istagclose ; istagopen = c=='<' ; istagclose = c=='>'
