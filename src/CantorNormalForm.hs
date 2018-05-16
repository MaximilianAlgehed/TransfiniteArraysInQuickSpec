{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module CantorNormalForm where

import Prelude hiding (pred)

import GHC.Generics
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Ord
import Numeric.Natural

{- Properties of the ordinals -}
prop_add_assoc :: Ordinal -> Ordinal -> Ordinal -> Bool
prop_add_assoc a b c = a + (b + c) == (a + b) + c

prop_mul_assoc :: Ordinal -> Ordinal -> Ordinal -> Bool
prop_mul_assoc a b c = a * (b * c) == (a * b) * c

{- Definitions -}

data Ordinal = N { finPa :: Natural }
             | O { expo  :: Ordinal
                 , coeff :: Natural
                 , rst   :: Ordinal }
             deriving (Eq, Generic, CoArbitrary)

instance Show Ordinal where
  show (N a) = show a
  show (O e c r) = "w" ++ exponent ++ coeff ++ remainder
    where
      remainder = if r == 0 then "" else " + " ++ show r
      coeff = if c == 1 then "" else "*" ++ show c
      exponent
        | e == 1 = ""
        | otherwise = "^" ++ if noBrackets then show e else "(" ++ show e ++ ")"
      noBrackets = finite e || len e == 1 && limitOrdinal e

instance Arbitrary Ordinal where
  arbitrary = sized (go . floor . log . fromIntegral)
    where
      go 0 = oneof [ N <$> arbitrary, return w ]
      go s = frequency [(10, do
        len    <- abs <$> arbitrary
        alphas <- reverse . filter (>0) . nub . sort <$> sequence (replicate len (go (s `div` len)))
        xs     <- sequence [ (+1) <$> arbitrary | _ <- alphas ]
        p      <- N <$> arbitrary
        return $ foldr (\(a, x) rst -> O a x rst) p (zip alphas xs)), (1, N <$> arbitrary)]

w :: Ordinal
w = O 1 1 (N 0)

len :: Ordinal -> Natural
len o = if finite o then 0 else 1 + len (rst o)

finite :: Ordinal -> Bool
finite (N _) = True
finite _     = False

finitePart :: Ordinal -> Natural
finitePart o = if finite o then finPa o else finitePart (rst o)

fe :: Ordinal -> Ordinal
fe o = if finite o then 0 else expo o 

fco :: Ordinal -> Natural
fco o = if finite o then finitePart o else coeff o

instance Ord Ordinal where
  compare a b
    | finite a && finite b = compare (finitePart a) (finitePart b)
    | finite a             = LT
    | finite b             = GT
    | fe a /= fe b         = compare (fe a) (fe b)
    | fco a /= fco b       = compare (fco a) (fco b)
    | otherwise            = compare (rst a) (rst b)

{- Exponentiation -}
pow1 :: Natural -> Ordinal -> Ordinal
pow1 k a
  | fe a == 1      = O (N $ fco a) (k ^ finitePart a) 0
  | finite (rst a) = O (O (fe a - 1) (fco a) 0) (k ^ finitePart a) 0
  | otherwise      = O (O (fe a - 1) 1 (fe c)) (fco c) 0
                     where c = pow1 k (rst a)

pred :: Natural -> Natural
pred n = if n == 0 then 0 else n - 1

-- Raising a limit ordinal to a power
pow2 :: Ordinal -> Natural -> Ordinal
pow2 a k
  | k == 0 = 1
  | otherwise = a * pow2 a (pred k)

limitPart :: Ordinal -> Ordinal
limitPart a
  | finite a  = 0
  | otherwise = O (fe a) (fco a) (limitPart (rst a))

limitOrdinal :: Ordinal -> Bool
limitOrdinal a = not (finite a) && finitePart a == 0

padd :: Ordinal -> Ordinal -> Natural -> Ordinal
padd a b n
  | n == 0 = a + b
  | otherwise = O (fe a) (fco a) (padd (rst a) b (pred n))

pow3h :: Ordinal -> Ordinal -> Natural -> Natural -> Ordinal
pow3h a p n k
  | k == 1 = (a * p) + p
  | otherwise = padd (pow2 a k * p) (pow3h a p n (pred k)) n

pow3 :: Ordinal -> Natural -> Ordinal
pow3 a k
  | k == 1          = a
  | limitOrdinal a  = pow2 a k
  | otherwise       = padd (pow2 c k) (pow3h c (N (finitePart a)) n (pred k)) n
                      where
                        c = limitPart a
                        n = len a

pow4 :: Ordinal -> Ordinal -> Ordinal
pow4 a b = (O (fe a * limitPart b) 1 0) * pow3 a (finitePart b)

(^.) :: Ordinal -> Ordinal -> Ordinal
a ^. b
  | b == 0 || a == 1 = 1
  | a == 0           = 0
  | finite a && finite b = N (finitePart a ^ finitePart b)
  | finite a             = pow1 (finitePart a) b
  | finite b             = pow3 a (finitePart b)
  | otherwise            = pow4 a b

instance Num Ordinal where
  a + b
    | finite a && finite b = N (finitePart a + finitePart b)
    | fe a < fe b          = b
    | fe a == fe b         = O (fe a) (fco a + fco b) (rst b)
    | otherwise            = O (fe a) (fco a) (rst a + b)

  a - b
    | finite a && finite b = if finitePart a < finitePart b
                             then 0
                             else N (finitePart a - finitePart b)
    | fe a < fe b          = 0
    | fe a > fe b          = a
    | fco a < fco b        = 0
    | fco a > fco b        = O (fe a) (fco a - fco b) (rst a)
    | otherwise            = rst a - rst b

  a * b
    | a == 0 || b == 0     = 0 
    | finite a && finite b = N (finitePart a * finitePart b)
    | finite b             = O (fe a) (fco a * (finitePart b)) (rst a)
    | otherwise            = O (fe a + fe b) (fco b) (a * rst b)

  signum _                 = 1

  abs                      = id

  fromInteger              = N . fromInteger
