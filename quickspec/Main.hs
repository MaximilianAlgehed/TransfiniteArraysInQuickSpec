{-# LANGUAGE TypeApplications
           , MultiParamTypeClasses
           , FlexibleInstances
           , TypeOperators
           , FlexibleContexts
#-}
module Main where

import Prelude hiding (take, drop)

import Test.QuickCheck.Modifiers
import Test.QuickCheck
import QuickSpec
import Numeric.Natural

import Arrays
import CantorNormalForm

ordinals = 
  [ con "+" ((+) @Ordinal)
  , con "*" ((*) @Ordinal)
  , con "0" (0 :: Ordinal)
  , con "1" (1 :: Ordinal)
  , con "w" w
  , con "min" (min @Ordinal)
  --, con "max" (max @Ordinal)
  --, predicate "finite" finite
  , monoType (Proxy :: Proxy Ordinal)
  --, background [ con "&&" (&&)
  --             , con "True" True
  --             , con "False" False ]
  ]

instance Ord a => Observe [Ordinal] [a] (Arr Ordinal a) where
  observe inp xs
    | finite (size xs) =
      let sz = finitePart (size xs)
      in if sz == 0 then [] else [ xs ! (N i) | i <- [0 .. sz - 1] ]
    | otherwise = [xs ! i | i <- inp, i < size xs]

arrays = 
  [ con "[]"   (empty  :: Arr Ordinal Ordinal)
  , con "iota" (iota   :: Ordinal -> Arr Ordinal Ordinal)
  , con "hd"   (hd     :: Arr Ordinal A -> Maybe A)
  , con "tl"   (tl     :: Arr Ordinal A -> Arr Ordinal A)
  , con ":"    (cons   :: A -> Arr Ordinal A -> Arr Ordinal A)
  , con "snoc" (snoc   :: A -> Arr Ordinal A -> Arr Ordinal A)
  , con "++"   (append :: Arr Ordinal A -> Arr Ordinal A -> Arr Ordinal A)
  , con "drop" (drop   :: Ordinal -> Arr Ordinal A -> Arr Ordinal A)
  , con "take" (take   :: Ordinal -> Arr Ordinal A -> Arr Ordinal A)
  , con "size" (size   :: Arr Ordinal A -> Ordinal)
  , con "pull" (Pull   :: Ordinal -> (Ordinal -> A) -> Arr Ordinal A)
  , inst (Sub Dict :: Arbitrary A :- Arbitrary (Arr Ordinal A))
  , inst (Sub Dict :: Ord A :- Observe [Ordinal] [A] (Arr Ordinal A))
  , monoType (Proxy :: Proxy Natural)
  , background ordinals
  , withMaxTermSize 7
  ]

main = do
  quickSpec arrays
