{-# OPTIONS_GHC -Wall #-}

module Util where

import Control.Monad
import Data.Char

import qualified Data.List



(~.) = flip (.)

(~:) = flip ($)

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


butNot notval defval val |(val==notval) = defval |otherwise = val


ifNull val defval = if null val then defval else val

unlessNull testval goalval = if null testval then [] else goalval

unlessNullOp testval op = if null testval then [] else op testval

noNull = not.null

isnt notval = noNull.(butNot notval "")


repeatedly fn arg =
    let result = fn arg
    in if result==arg then result else repeatedly fn result


(#)::
    [t] -> Int -> t
--  alias for: `!!` ..for these most common cases, no need to `fold`
(x:_) # 0 = x
(_:x:_) # 1 = x
(_:_:x:_) # 2 = x
(_:_:_:x:_) # 3 = x
(_:_:_:_:x:_) # 4 = x
(_:_:_:_:_:x:_) # 5 = x
(_:_:_:_:_:_:x:_) # 6 = x
(_:_:_:_:_:_:_:x:_) # 7 = x
(_:_:_:_:_:_:_:_:x:_) # 8 = x
(_:_:_:_:_:_:_:_:_:x:_) # 9 = x
list@(_:_) # i = (drop i list) # 0 where
infix 9 #


-- for uses such as `crop` without (directly) taking the `length`
dropLast 0 = id
dropLast 1 = init
dropLast n = ((#n) . reverse . Data.List.inits)
-- dropLast n l = l~:take (l~:length - n)


takeLast 0 = const []
takeLast 1 = (:[]).last
takeLast n = (#n) . reverse . Data.List.tails


indexed l = zip [0 .. ] l

crop 0 0 = id
crop 0 1 = init
crop 0 end = dropLast end
crop 1 0 = tail
crop start 0 = drop start
crop start end = (drop start) . (dropLast end)

contains :: (Eq t)=> [t]->[t]->Bool
contains = flip Data.List.isInfixOf

endsWith :: (Eq t)=> [t]->[t]->Bool
endsWith = flip Data.List.isSuffixOf

startsWith :: (Eq t)=> [t]->[t]->Bool
startsWith = flip Data.List.isPrefixOf

toLower = (>~ Data.Char.toLower)

toUpper = (>~ Data.Char.toUpper)

join = Data.List.intercalate

subAt start len = (take len) . (drop start)

substitute old new
    |(old==new) = id
    |(otherwise) = (>~ subst) where
        subst item |(item==old) = new |(otherwise) = item

trim = trim'' Data.Char.isSpace
trim' dropitems = trim'' (`elem` dropitems)
trim'' fn = (trimStart'' fn) ~. (trimEnd'' fn)

trimEnd = trimEnd'' Data.Char.isSpace
trimEnd' dropitems = trimEnd'' (`elem` dropitems)
trimEnd'' = Data.List.dropWhileEnd

trimStart = trimStart'' Data.Char.isSpace
trimStart' dropitems = trimStart'' (`elem` dropitems)
trimStart'' = Data.List.dropWhile


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
    |(index > -1 && lengthGt index list) = list#index
    |(otherwise) = defval


lengthGEq 0 = const True
lengthGEq n = noNull . drop (n - 1)

lengthGt 0 = noNull
lengthGt n = noNull . drop n


fuseElems is2fuse fusion (this:next:more) =
    (fused:rest) where
        nofuse = not$ is2fuse this next
        fused = if nofuse then this else fusion this next
        rest = fuseElems is2fuse fusion$
                if nofuse then (next:more) else more
fuseElems _ _ l = l


indexOfSub _ [] = minBound::Int
indexOfSub sub str@(_:rest)
    |(zip sub str) ~: (all $(==)~:uncurry)
        = 0
    |otherwise
        = 1+(indexOfSub sub rest)

lastIndexOfSub _ _ [] = minBound::Int
lastIndexOfSub rev sub str
    |(idx<0) = minBound::Int
    |(otherwise) = (str~:length)-(sub~:length)-idx
    where idx = indexOfSub (rev sub) (rev str)


splitBy delim =
    foldr foreach [[]] where
        foreach _ [] = []
        foreach item items@(item0:rest)
            |(item==delim) = []:items
            |(otherwise) = (item:item0):rest



splitUp beginners =
    _splitup (length$ atOr beginners 0 "") lastidx beginners where
        lastidx' = lastIndexOfSub id
        lastidx bstr = (beginners>~ \each-> lastidx' each bstr) ~: maximum


_splitup _ _ _ _ "" = []
_splitup 0 _ _ _ s = [("",s)]
_splitup _ _ [] _ s = [("",s)]
_splitup _ _ _ "" s = [("",s)]
_splitup beg0len lastidx beginners end str =
    (tolist pre "") ++ (tolist match beginner) ++ --  only recurse if we have a good reason:
        (if nomatch && splitat==0 then (tolist rest "") else (_splitup beg0len lastidx beginners end rest))
    where
    pre = str ~: (take$ if nomatch then splitat else begpos)
    match = if nomatch then "" else str ~: (take endpos) ~: (drop $begpos+beg0len)
    rest = str ~: (drop$ if nomatch then splitat else endposl)
    beginner = if nomatch then "" else str ~: (take endpos) ~: (drop begpos) ~: take beg0len
    nomatch = endpos<0 || begpos<0
    splitat = if nomatch && endpos>=0 then endposl else 0
    endpos = indexOfSub end str
    begpos = if endpos<0 then -1 else
        lastidx$ str ~: (take endpos) ~: reverse
    endposl = endpos+(end~:length)
    tolist val beg = unlessNull val [(val,beg)]
