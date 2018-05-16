module Arrays where

import Test.QuickCheck

data Arr i a = Pull { size :: i, (!) :: i -> a }

instance (Arbitrary i, CoArbitrary i, Arbitrary a) => Arbitrary (Arr i a) where
  arbitrary = Pull <$> arbitrary <*> arbitrary

instance Functor (Arr i) where
  fmap f xs = Pull (size xs) (f . (xs!))

empty :: Num i => Arr i a
empty = Pull 0 undefined

iota :: i -> Arr i i
iota i = Pull i id

hd :: (Eq i, Num i) => Arr i a -> Maybe a
hd xs = if size xs == 0 then Nothing else Just (xs ! 0)

tl :: Num i => Arr i a -> Arr i a
tl xs = Pull (size xs - 1) (\i -> xs ! (i + 1))

cons :: (Eq i, Num i) => a -> Arr i a -> Arr i a
cons x xs = Pull (1 + size xs) (\i -> if i == 0 then x else xs ! (i - 1))

snoc :: (Eq i, Num i) => a -> Arr i a -> Arr i a
snoc x xs = Pull (size xs + 1) (\i -> if i == size xs then x else xs ! i)

append :: (Ord i, Num i) => Arr i a -> Arr i a -> Arr i a
append xs ys = Pull (size xs + size ys) (\i -> if i < size xs then xs ! i else ys ! (i - size xs))

drop :: Num i => i -> Arr i a -> Arr i a
drop i xs = Pull (size xs - i) (\idx -> xs ! (i + idx))

take :: (Ord i, Num i) => i -> Arr i a -> Arr i a
take i xs = Pull (min (size xs) i) (xs!)
