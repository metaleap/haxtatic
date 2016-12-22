{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NegativeLiterals #-}

module Util where



($>) = flip ($)


(#)::
    [t] -> Int -> t
--  alias for: `!!` ..for these most common cases, no need to `fold`
(#) (x:_) 0 = x
(#) (_:x:_) 1 = x
(#) (_:_:x:_) 2 = x
(#) (_:_:_:x:_) 3 = x
(#) (_:_:_:_:x:_) 4 = x
(#) (_:_:_:_:_:x:_) 5 = x
(#) (_:_:_:_:_:_:x:_) 6 = x
(#) (_:_:_:_:_:_:_:x:_) 7 = x
(#) (_:_:_:_:_:_:_:_:x:_) 8 = x
(#) (_:_:_:_:_:_:_:_:_:x:_) 9 = x
(#) l i = l!!i
infix 9 #


(°)::
    Foldable f =>
    f l->
    Int
--  alias for: `length` ..as it really adds up over time
(°) = length


atOr::
    [t]-> Int-> t->
    t
--  value in `list` at `index`, or `defval`
atOr [] _ defval = defval
atOr (_:[]) 1 defval = defval
atOr (_:_:[]) 2 defval = defval
atOr (_:_:_:[]) 3 defval = defval
atOr (_:_:_:_:[]) 4 defval = defval
atOr (_:_:_:_:_:[]) 5 defval = defval
atOr list index defval = if (°)list > index then list#index else defval
