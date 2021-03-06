{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-}
module CantorNormalForm where

import Prelude hiding (pred)

import GHC.Stack
import GHC.Generics
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Ord
import Numeric.Natural

{- Definitions -}

data Ordinal = N { finPa :: Natural }
             | O { expo  :: Ordinal
                 , coeff :: Natural
                 , rst_  :: Ordinal }
             deriving (Eq, Generic, CoArbitrary)

rst :: HasCallStack => Ordinal -> Ordinal
rst (N _) = error "rst applied badly!"
rst o = rst_ o

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

  shrink (N n) = N <$> shrink n
  shrink (O expo coeff rst) = [rst] ++ shrink rst ++ [ O expo c rst | c <- shrink coeff, c > 0 ]
                            ++ [ O e coeff rst | e <- shrink expo, e > fe rst ]

w :: HasCallStack => Ordinal
w = O 1 1 (N 0)

len :: HasCallStack => Ordinal -> Natural
len o = if finite o then 0 else 1 + len (rst o)

finite :: HasCallStack => Ordinal -> Bool
finite (N _) = True
finite _     = False

finitePart :: HasCallStack => Ordinal -> Natural
finitePart o = if finite o then finPa o else finitePart (rst o)

fe :: HasCallStack => Ordinal -> Ordinal
fe o = if finite o then 0 else expo o 

fco :: HasCallStack => Ordinal -> Natural
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
pred :: HasCallStack => Natural -> Natural
pred n = if n == 0 then 0 else n - 1

limitPart :: HasCallStack => Ordinal -> Ordinal
limitPart a
  | finite a  = 0
  | otherwise = O (fe a) (fco a) (limitPart (rst a))

limitOrdinal :: HasCallStack => Ordinal -> Bool
limitOrdinal a = not (finite a) && finitePart a == 0

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
