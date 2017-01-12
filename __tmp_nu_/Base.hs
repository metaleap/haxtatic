{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Base where

import qualified Control.Monad
import qualified Data.Maybe
import Data.Function ( (&) )



is ::
    (Foldable f)=>
    f a -> Bool

is = not.null



when ::
    Bool -> a -> a -> a

when True v _ = v
when False _ v = v



infixl ~.
(~.) = flip (.)



infixl 9 -:
(-:) = (&)

infixl ~>
(~>) = (&)



infix 0 =:
(=:) = (,)

--  (=:=)  :: Eq eq => (eq, eq) -> Bool
--  (=:=) = uncurry (==)




(>~) ::
    (Functor f)=>
    f a -> (a -> b) -> f b
infixl 9 >~
(>~) = flip fmap




infixr 7 |~
(|~) = filter

infixl 7 ~|
(~|) = flip filter




(>>~) ::
    (Traversable t, Monad m)=>
    t a -> (a -> m b) -> m (t b)
infixl 8 >>~
(>>~) = Control.Monad.forM




(>>|) ::
    (Applicative m)=>
    [a] -> (a -> m Bool) -> m [a]
infixl >>|
(>>|) = flip Control.Monad.filterM





(|?) ::
    Bool -> a -> a -> a
infix 1 |?
(|?) = when


infixr 0 |!
(|!) = ($)


infix 1 -|=
(-|=) = Data.Maybe.fromMaybe

infix 0 =|-
(=|-) = flip maybe



(@!) ::
    [a] -> Int -> a
infixl 9 @!
--  alias for: `!!`
[] @! _ = undefined  --  rids this Careful Coder (TM) of the pesky 'non-exhaustive patterns' warning
(x:_) @! 0 = x
(_:x:_) @! 1 = x
-- (_:_:x:_) @! 2 = x
-- (_:_:_:x:_) @! 3 = x
-- (_:_:_:_:x:_) @! 4 = x
--  these above are all branches so release only as necessary
list @! i = (drop i list) @! 0



(@?) ::
    [a] -> Int -> Maybe a
infixl 9 @?
[] @? _ = Nothing
(x:_) @? 0 = Just x
(_:x:_) @? 1 = Just x
-- (_:_:x:_) @? 2 = Just x
-- (_:_:_:x:_) @? 3 = Just x
-- (_:_:_:_:x:_) @? 4 = Just x
--  these above are all branches so release only as necessary
list @? i = (drop i list) @? 0
