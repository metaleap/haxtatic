{-# OPTIONS_GHC -Wall #-}
module Html where


import qualified Util


data Tag = T {
        name :: String,
        attr :: Util.KeyVals,
        sub :: [Tag]
    } deriving (Eq, Read)


escapes :: Util.KeyVals
escapes = [("\"","&quot;"),("'","&apos;"),(">","&gt;"),("<","&lt;"),("&","&amp;")]


emit::
    Tag->
    String
emit tag = if notag then inner else open++atts++inner++close where
    open = "<"++tname
    atts = concat $ map emitatt $ tatts where
        emitatt (n,v) = if (n=="" || v=="") then "" else (" "++n++"=\""++v++"\"")
    inner = (if notag then "" else if noinner then "/>\n" else ">"++(if not $ null tinner then "" else "\n"))++(concat $ map emit tchildren)++tinner
    close = if noinner then "" else ("</"++tname++">\n")

    notag = null tname
    tname = name tag
    tatts = attr tag
    tinner = if (tatts/=[] && att0n=="") then att0v else "" where att0n = fst att0 ; att0v = snd att0 ; att0 = head tatts
    tchildren = sub tag
    noinner = tchildren==[] && tinner==""


noTag::
    Tag->
    Bool
noTag tag = null $ name tag


out::
    String-> Util.KeyVals-> [Tag]->
    String
out tname tatts tchildren = emit $ T tname tatts tchildren


-- HACKY! if line starts with <name> and ends with </name>, get inner text, else empty
t :: String->String
t n = "<"++n++">"
t' :: String->String
t' n = "</"++n++">"
t2 :: String->String
t2 n = "<"++n++" "
t2' :: String->String
t2' _ = "/>"
t3 :: String->String
t3 n = "<"++n++" "
t3' :: String->String
t3' n = "</"++n++">"
tagInner::
    String-> String->
    String
tagInner tn line =
    let ll = length line ; lt = length tn ; ls = lt+2 ; le = lt+3
        in if ll>(ls+le) && ((t tn)==(take ls line)) && ((t' tn)==(drop (ll-le) line))
            then drop ls (take (ll-le) line) else ""
tagInner2::
    String-> String->
    String
tagInner2 tn line =
    let ll = length line ; lt = length tn ; ls = lt+2 ; le = 2
        in if ll>(ls+le) && ((t2 tn)==(take ls line)) && ((t2' tn)==(drop (ll-le) line))
            then drop ls (take (ll-le) line) else ""
tagInner3::
    String-> String->
    String
tagInner3 tn line =
    let ll = length line ; lt = length tn ; ls = lt+2 ; le = lt+3
        in if ll>(ls+le) && ((t3 tn)==(take ls line)) && ((t3' tn)==(drop (ll-le) line))
            then drop ls (take (ll-le) line) else ""

tagInnerPlain::
    String->
    String
tagInnerPlain markup = striptags False markup where
    striptags _ [] = []
    striptags intagwas (c:rest) = char:(striptags intagnext rest) where
        char = if intagnow then ' ' else c; intagnow = intagwas || istagopen || istagclose ; intagnext = intagnow && not istagclose ; istagopen = c=='<' ; istagclose = c=='>'
