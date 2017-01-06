{-# OPTIONS_GHC -Wall #-}
module Util where

import Base
import qualified Control.Monad
import qualified Data.Char
import qualified Data.List
import qualified Data.Time.Calendar
import qualified Data.Time.Clock
import Data.Function ( (&) )
import qualified Text.Read



type StringPairs = [(String , String)]



dateTime0 = Data.Time.Clock.UTCTime {
                Data.Time.Clock.utctDay = Data.Time.Calendar.ModifiedJulianDay {
                                            Data.Time.Calendar.toModifiedJulianDay = 0 },
                Data.Time.Clock.utctDayTime = 0
            }
_intmin = minBound::Int



both (fun1,fun2) (tfst,tsnd) =
    (fun1 tfst , fun2 tsnd)

both' f = both (f,f)


bothFsts fn (fst1,_) (fst2,_) =
    fn fst1 fst2

bothSnds fn (_,snd1) (_,snd2) =
    fn snd1 snd2

butNot notval defval val
    |(val==notval)= defval
    |(otherwise)= val


ifNo val defval =
    if null val then defval else val

ifIs testval op =
    if null testval then [] else op testval


noneOf vals val =
    all (val/=) vals


repeatedly fn arg =
    let result = fn arg
    in if (result==arg) then result else repeatedly fn result


via fn =
    --  use it to `>>=` a value over to fn but then
    --  discard its return and return the value instead
    ((>>)fn).return



-- for uses such as `crop` without (directly) taking the `length`
dropLast 0 = id
dropLast 1 = init
dropLast n = (#n) . reverse . Data.List.inits
-- dropLast n l = l~>take (l~>length - n)


takeLast 0 = const []
takeLast 1 = (:[]).last
takeLast n = (#n) . reverse . Data.List.tails


indexed l =
    zip [0 .. ] l

crop 0 0 = id
crop 0 1 = dropLast 1
crop 0 end = dropLast end
crop 1 0 = drop 1 -- `tail` could error out, one less worry
crop start 0 = drop start
crop start end =
    (drop start) . (dropLast end)

count _ [] = 0
count item (this:rest) =
    (if item==this then 1 else 0) + (count item rest)

countSub _ [] = 0
countSub [] _ = 0
countSub list sub =
    foldr each 0 (Data.List.tails list) where
        each listtail counter =
            if Data.List.isPrefixOf sub listtail then counter + 1 else counter

countAnySubs list subs =
    --  equivalent to, but faster than:
    --      sum (map (countSub list) subs)
    foldr each 0 (Data.List.tails list) where
    each listtail counter =
        if any isprefix subs then counter + 1 else counter where
        isprefix sub = Data.List.isPrefixOf sub listtail

countSubVsSubs list (sub,subs) =
    --  equivalent to, but faster than:
    --      (countSub list sub, countAnySubs list subs)
    foldr each (0,0) (Data.List.tails list) where
    each listtail count =
        let isprefix sub = Data.List.isPrefixOf sub listtail
            isp1 = isprefix sub
            isp2 = any isprefix subs
        in if isp1 || isp2
            then let (c1,c2) = count in
                    (if isp1 then c1 + 1 else c1,
                    if isp2 then c2 + 1 else c2)
            else count



contains :: (Eq t)=> [t] -> [t] -> Bool
contains = flip Data.List.isInfixOf

endsWith :: (Eq t)=> [t] -> [t] -> Bool
endsWith = flip Data.List.isSuffixOf

startsWith :: (Eq t)=> [t] -> [t] -> Bool
startsWith = flip Data.List.isPrefixOf

toLower :: String -> String
toLower = (>~ Data.Char.toLower)

toUpper :: String -> String
toUpper = (>~ Data.Char.toUpper)

join = Data.List.intercalate

lookup key defval = (defval -|=) . (Data.List.lookup key)

subAt start len =
    (take len) . (drop start)

substitute old new
    |(old==new)= id
    |(otherwise)= (>~ subst) where
        subst item |(item==old)= new |(otherwise)= item

trim = trim'' Data.Char.isSpace
trim' dropitems = trim'' (`elem` dropitems)
trim'' fn = (trimStart'' fn) ~. (trimEnd'' fn)
trimSpaceOr dropitems = trim'' (\c -> Data.Char.isSpace c || elem c dropitems )

trimEnd = trimEnd'' Data.Char.isSpace
trimEnd' dropitems = trimEnd'' (`elem` dropitems)
trimEnd'' = Data.List.dropWhileEnd

trimStart = trimStart'' Data.Char.isSpace
trimStart' dropitems = trimStart'' (`elem` dropitems)
trimStart'' = Data.List.dropWhile


tryParse nullval errval toparsestr str =
    if (null str) then nullval else
        errval -|= (Text.Read.readMaybe$ toparsestr str)

tryParseOr defval =
    (defval -|=).Text.Read.readMaybe



unMaybes list =
    (list ~|(/=Nothing)) >~ foreach where
    foreach (Just val) = val
    foreach _ = undefined



unique:: (Eq a)=> [a] -> [a]
unique = Data.List.nub

uniqueFst:: (Eq f)=> [(f,s)] -> [(f,s)]
uniqueFst = Data.List.nubBy (bothFsts (==))

uniqueSnd:: (Eq s)=> [(f,s)] -> [(f,s)]
uniqueSnd = Data.List.nubBy (bothSnds (==))


atOr::
    [t]  ->  Int  ->  t  ->
    t
--  value in `list` at `index`, or `defval`
atOr [] _ defval = defval
atOr (x:_) 0 _ = x
atOr (_:x:_) 1 _ = x
--  atOr (_:_:x:_) 2 _ = x
--  atOr (_:_:_:x:_) 3 _ = x
--  atOr (_:[]) 1 defval = defval
--  atOr (_:[]) 2 defval = defval
--  atOr (_:[]) 3 defval = defval
--  atOr (_:[]) 4 defval = defval
--  atOr (_:[]) 5 defval = defval
--  atOr (_:_:[]) 2 defval = defval
--  atOr (_:_:[]) 3 defval = defval
--  atOr (_:_:[]) 4 defval = defval
--  atOr (_:_:[]) 5 defval = defval
--  atOr (_:_:_:[]) 3 defval = defval
--  atOr (_:_:_:[]) 4 defval = defval
--  atOr (_:_:_:[]) 5 defval = defval
--  atOr (_:_:_:_:[]) 4 defval = defval
--  atOr (_:_:_:_:[]) 5 defval = defval
--  atOr (_:_:_:_:_:[]) 5 defval = defval
--  these above are all branches so release only as necessary
atOr list index defval
    |(index > -1 && lengthGt index list)= list#index
    |(otherwise)= defval


lengthGEq 0 = const True
lengthGEq n = is . drop (n - 1)

lengthGt 0 = is
lengthGt n = is . drop n

excerpt maxlen str =
    if s==str then s else (s++"...")
    where s = take maxlen str

fuseElems is2fuse fusion (this:next:more) =
    (fused:rest) where
        nofuse = not$ is2fuse this next
        fused = nofuse |? this |! fusion this next
        rest = fuseElems is2fuse fusion$
                nofuse |? (next:more) |! more
fuseElems _ _ l = l


indexOf _ [] =
    _intmin
indexOf item (this:rest) =
    if this==item then 0 else
        (1 + (indexOf item rest))

_indexof_droptil delim = _indexof_droptil' (delim==)
_indexof_droptil' _ _ [] =
    (_intmin , [])
_indexof_droptil' predicate count list@(this:rest) =
    if (predicate this) then (count , list)
        else _indexof_droptil' predicate (count + 1) rest

indexOf1st =
    indexof1st 0
    where
    indexof1st _ [] _ = (_intmin , [])
    indexof1st _ _ [] = (_intmin , [])
    indexof1st count items list@(this:rest) =
        if elem this items then (count , list) else
            indexof1st (count + 1) items rest

indexOfSub [] _ =
    _intmin
indexOfSub _ [] =
    _intmin
indexOfSub haystack needle =
    let (startindex , _haystack) = _indexof_droptil (needle#0) 0 haystack
    in if startindex<0 then startindex else
        startindex + indexofsub _haystack needle
        where
        indexofsub [] _ =
            _intmin
        indexofsub list@(_:rest) sub =
            if Data.List.isPrefixOf sub list then 0
                else (1 + indexofsub rest sub)
--  indexOfSub list@(_:rest) sub =
--      if Data.List.isPrefixOf sub list then 0
--          else (1 + indexOfSub rest sub)


indexOfSubs1st [] _ =
    (_intmin , "" )
indexOfSubs1st _ [] =
    (_intmin , "" )
indexOfSubs1st subs str =
    let iidxs = isubs >~ (both (id,indexof)) ~|snd~.(>=0)
        isubs = indexed subs
        indexof = indexOfSub _haystack
        (startindex , _haystack) = indexOf1st (subs >~ (#0)) str
        (i,index) = Data.List.minimumBy (bothSnds compare) iidxs
    in if startindex<0 || null iidxs || index<0
        then ( _intmin , "" )
        else ( index+startindex , subs#i )

lastIndexOfSub revstr revsub
    |(idx<0)= idx
    |(otherwise)= revstr~>length - revsub~>length - idx
    where
        idx = indexOfSub revstr revsub



replace old new str =
    _replace_helper (replace old new) (const new) (idx,old) str
    where
    idx = indexOfSub str old

replaceAny olds tonew str =
    _replace_helper (replaceAny olds tonew) tonew (indexOfSubs1st olds str) str

replaceAll [] = id
replaceAll ((old,new):[]) = replace old new
replaceAll replpairs =
    _replaceall
    where
    tonew ((oldval,newval):rest) old =
        if old==oldval then newval else tonew rest old
    tonew _ _ =
        error "Well somehow this codebase got messed up badly now!" -- OK to crash here, means a bug in the codebase
    (olds,_) = unzip replpairs
    _replhelper = _replace_helper _replaceall (tonew replpairs)
    _replaceall [] = []
    _replaceall str =
        _replhelper (indexOfSubs1st olds str) str

_replace_helper recurse tonew (idx,old) str =
    if idx<0 then str else
        let pre = take idx str
            rest = drop (idx + old~>length) str
        in pre ++ tonew old ++ recurse rest



splitOn delim =
    splitOn' (delim==)
splitOn' predicate =
    foldr each [[]] where
        each _ [] = []
        each item accum@(item0:rest)
            |(predicate item)= []:accum
            |(otherwise)= (item:item0):rest

splitOn1st delim =
    splitOn1st' (delim==)
splitOn1st' predicate list =
    let (i , rest) = _indexof_droptil' predicate 0 list
    in if i<0 then (list , [])
        else (take i list , drop 1 rest)
splitOn1stSpace = splitOn1st' Data.Char.isSpace



splitUp _ _ "" = []
splitUp _ "" src = [(src,"")]
splitUp [] _ src = [(src,"")]
splitUp allbeginners end src =
    (null beginners) |? [(src,"")] |! _splitup src
    where
    beginners' = allbeginners>~reverse ~|is
    beg0len = (beginners'#0)~>length
    beginners = beginners' ~| length~.((==)beg0len)

    lastidx revstr = (beginners>~ (lastIndexOfSub revstr)) ~> maximum

    _splitup str =
        (tolist "" pre) ++ (tolist beginner match) ++ --  only recurse if we have a good reason:
            (nomatch && splitat==0 |? (tolist "" rest) |! (_splitup rest))
        where
        pre = str ~> (take$ nomatch |? splitat |! begpos)
        match = nomatch |? "" |! (str ~> (take endpos) ~> (drop $begpos+beg0len))
        rest = str ~> (drop$ nomatch |? splitat |! endposl)
        beginner = nomatch |? "" |! str ~> (take endpos) ~> (drop begpos) ~> (take beg0len)
        nomatch = endpos<0 || begpos<0
        splitat = (nomatch && endpos>=0) |? endposl |! 0
        endpos = indexOfSub str end
        begpos = endpos<0 |? -1 |! lastidx$ str ~> (take endpos) ~> reverse
        endposl = endpos + (end~>length)
        tolist beg val = (null val && null beg) |? [] |! [(val,beg)]
