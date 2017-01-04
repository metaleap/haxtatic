{-# OPTIONS_GHC -Wall #-}
module Util where

import qualified Control.Monad
import qualified Data.Char
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Time.Calendar
import qualified Data.Time.Clock
import Data.Function ( (&) )



type StringPairs = [(String , String)]



dateTime0 = Data.Time.Clock.UTCTime {
                Data.Time.Clock.utctDay = Data.Time.Calendar.ModifiedJulianDay {
                                            Data.Time.Calendar.toModifiedJulianDay = 0 },
                Data.Time.Clock.utctDayTime = 0
            }



(~.) = flip (.)

(~:) = (&)

(=:) = (,)
infix 4 =:

-- LAST: (>~) :: Functor f => f a -> (a -> b) -> f b
(>~) = flip fmap
infix 8 >~

(|~) = filter
infix 7 |~

(~|) = flip filter
infix 7 ~|

(>>~) :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
(>>~) = Control.Monad.forM
infix 8 >>~

(>>|) :: Applicative m => [a] -> (a -> m Bool) -> m [a]
(>>|) = flip Control.Monad.filterM

(|?) :: Bool -> a -> a -> a
(|?) = when
infixl 1 |?

(|!) = ($)
infixr 0 |!

when True v _ = v
when False _ v = v


both (f1,f2) (p1,p2) =
    (f1 p1 , f2 p2)

both' f = both (f,f)


both1st fn (fst1,_) (fst2,_) =
    fn fst1 fst2

both2nd fn (_,snd1) (_,snd2) =
    fn snd1 snd2

butNot notval defval val
    |(val==notval)= defval
    |(otherwise)= val


ifNull val defval =
    if null val then defval else val

unlessNull testval goalval =
    if null testval then [] else goalval

unlessNullOp testval op =
    if null testval then [] else op testval

noNull = not.null

noneOf vals val =
    all (val/=) vals

isnt notval =
    noNull.(butNot notval "")


repeatedly fn arg =
    let result = fn arg
    in (result==arg) |? result |! repeatedly fn result


via fn =
    --  to `>>=` something into a typically `>>` func such as print
    ((>>)fn).return


(#)::
    [t] -> Int -> t
--  alias for: `!!` ..for these most common cases, no need to `fold`
[] #_ = undefined  --  rids this Careful Coder (TM) of the pesky 'non-exhaustive patterns' warning
(x:_) #0 = x
(_:x:_) #1 = x
(_:_:x:_) #2 = x
(_:_:_:x:_) #3 = x
(_:_:_:_:x:_) #4 = x
(_:_:_:_:_:x:_) #5 = x
(_:_:_:_:_:_:x:_) #6 = x
(_:_:_:_:_:_:_:x:_) #7 = x
(_:_:_:_:_:_:_:_:x:_) #8 = x
(_:_:_:_:_:_:_:_:_:x:_) #9 = x
list #i = (drop i list) #0
infix 9 #


-- for uses such as `crop` without (directly) taking the `length`
dropLast 0 = id
dropLast 1 = init
dropLast n = (#n) . reverse . Data.List.inits
-- dropLast n l = l~:take (l~:length - n)


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
    (item==this |? 1 |! 0) + (count item rest)


contains :: (Eq t)=> [t]->[t]->Bool
contains = flip Data.List.isInfixOf

endsWith :: (Eq t)=> [t]->[t]->Bool
endsWith = flip Data.List.isSuffixOf

startsWith :: (Eq t)=> [t]->[t]->Bool
startsWith = flip Data.List.isPrefixOf

toLower = (>~ Data.Char.toLower)

toUpper = (>~ Data.Char.toUpper)

join = Data.List.intercalate

lookup key defval = (Data.Maybe.fromMaybe defval) . (Data.List.lookup key)

subAt start len =
    (take len) . (drop start)

substitute old new
    |(old==new)= id
    |(otherwise)= (>~ subst) where
        subst item |(item==old)= new |(otherwise)= item

trim = trim'' Data.Char.isSpace
trim' dropitems = trim'' (`elem` dropitems)
trim'' fn = (trimStart'' fn) ~. (trimEnd'' fn)

trimEnd = trimEnd'' Data.Char.isSpace
trimEnd' dropitems = trimEnd'' (`elem` dropitems)
trimEnd'' = Data.List.dropWhileEnd

trimStart = trimStart'' Data.Char.isSpace
trimStart' dropitems = trimStart'' (`elem` dropitems)
trimStart'' = Data.List.dropWhile

unique:: (Eq a)=> [a]-> [a]
unique = Data.List.nub


atOr::
    [t]-> Int-> t->
    t
--  value in `list` at `index`, or `defval`
atOr [] _ defval = defval
atOr (x:_) 0 _ = x
atOr (_:x:_) 1 _ = x
atOr (_:_:x:_) 2 _ = x
atOr (_:_:_:x:_) 3 _ = x
atOr (_:[]) 1 defval = defval
atOr (_:[]) 2 defval = defval
atOr (_:[]) 3 defval = defval
atOr (_:[]) 4 defval = defval
atOr (_:[]) 5 defval = defval
atOr (_:_:[]) 2 defval = defval
atOr (_:_:[]) 3 defval = defval
atOr (_:_:[]) 4 defval = defval
atOr (_:_:[]) 5 defval = defval
atOr (_:_:_:[]) 3 defval = defval
atOr (_:_:_:[]) 4 defval = defval
atOr (_:_:_:[]) 5 defval = defval
atOr (_:_:_:_:[]) 4 defval = defval
atOr (_:_:_:_:[]) 5 defval = defval
atOr (_:_:_:_:_:[]) 5 defval = defval
atOr list index defval
    |(index > -1 && lengthGt index list)= list#index
    |(otherwise)= defval


lengthGEq 0 = const True
lengthGEq n = noNull . drop (n - 1)

lengthGt 0 = noNull
lengthGt n = noNull . drop n


fuseElems is2fuse fusion (this:next:more) =
    (fused:rest) where
        nofuse = not$ is2fuse this next
        fused = nofuse |? this |! fusion this next
        rest = fuseElems is2fuse fusion$
                nofuse |? (next:more) |! more
fuseElems _ _ l = l


indexOf _ [] =
    minBound::Int
indexOf item (this:rest) =
    (this==item) |? 0 |! 1 + (indexOf item rest)


indexOfSub [] _ =
    minBound::Int
indexOfSub str@(_:rest) sub
    | (zip sub str) ~: (all $(==)~:uncurry)
    = 0
    | otherwise
    = 1 + (indexOfSub rest sub)
    --  --this dumb 1+ DOES seem slightly faster
    --  --than compare-lt + conditional-add, so NOT this:
    --  = idx (indexOfSub sub rest)
    --  where
    --      idx i |i<0 = i |otherwise = 1+i

indexOfSubs1st str subs =
    let isubs = indexed subs
        indexof = indexOfSub str
        iidxs = isubs >~ (both (id,indexof)) ~|snd~.(>=0)
        (i,index) = Data.List.minimumBy (both2nd compare) iidxs
    in (null iidxs)
        |? (minBound::Int , "")
        |! (index , subs#i)

lastIndexOfSub revstr revsub
    |(idx<0)= idx
    |(otherwise)= revstr~:length - revsub~:length - idx
    where
        idx = indexOfSub revstr revsub



replace old new str =
    _replace_helper idx old (const new) (replace old new) str
    where idx = indexOfSub str old

replaceAny olds tonew str =
    _replace_helper idx old tonew (replaceAny olds tonew) str
    where (idx,old) = indexOfSubs1st str olds

_replace_helper idx old tonew replrest str =
    idx<0 |? str |!
        pre ++ (tonew old) ++ replrest rest where
            pre = take idx str
            rest = drop (idx + old~:length) str



splitAt1st delim list =
    i<0 |? (list , []) |! (one , drop 1 two) where
        i = indexOf delim list
        (one,two) = Data.List.splitAt i list


splitBy delim =
    foldr foreach [[]] where
        foreach _ [] = []
        foreach item items@(item0:rest)
            |(item==delim)= []:items
            |(otherwise)= (item:item0):rest



splitUp _ _ "" = []
splitUp _ "" src = [(src,"")]
splitUp [] _ src = [(src,"")]
splitUp allbeginners end src =
    (null beginners) |? [(src,"")] |! _splitup src
    where
    beginners' = allbeginners>~reverse ~|noNull
    beg0len = (beginners'#0)~:length
    beginners = beginners' ~| length~.((==)beg0len)

    lastidx revstr = (beginners>~ (lastIndexOfSub revstr)) ~: maximum

    _splitup str =
        (tolist pre "") ++ (tolist match beginner) ++ --  only recurse if we have a good reason:
            (nomatch && splitat==0 |? tolist rest "" |! _splitup rest)
        where
        pre = str ~: (take$ nomatch |? splitat |! begpos)
        match = nomatch |? "" |! (str ~: (take endpos) ~: (drop $begpos+beg0len))
        rest = str ~: (drop$ nomatch |? splitat |! endposl)
        beginner = nomatch |? "" |! str ~: (take endpos) ~: (drop begpos) ~: take beg0len
        nomatch = endpos<0 || begpos<0
        splitat = (nomatch && endpos>=0) |? endposl |! 0
        endpos = indexOfSub str end
        begpos = endpos<0 |? -1 |! lastidx$ str ~: (take endpos) ~: reverse
        endposl = endpos + (end~:length)
        tolist val beg = (null val && null beg) |? [] |! [(val,beg)]
