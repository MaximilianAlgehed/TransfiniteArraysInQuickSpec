{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.QuickCheck
import System.Exit
import Control.Monad

import CantorNormalForm

{- Properties of the ordinals -}
prop_add_assoc :: Ordinal -> Ordinal -> Ordinal -> Bool
prop_add_assoc a b c = a + (b + c) == (a + b) + c

prop_add_right_monotonicity :: Ordinal -> Ordinal -> Ordinal -> Property
prop_add_right_monotonicity a b c = b < c ==> a + b < a + c

prop_add_weak_left_monotonicity :: Ordinal -> Ordinal -> Ordinal -> Property
prop_add_weak_left_monotonicity a b c = b < c ==> b + a <= c + a

prop_mul_assoc :: Ordinal -> Ordinal -> Ordinal -> Bool
prop_mul_assoc a b c = a * (b * c) == (a * b) * c

prop_mul_left_absorption :: Ordinal -> Property
prop_mul_left_absorption n = 0 < n && n < w ==> n * w == w

prop_mul_right_monotonicity :: Ordinal -> Ordinal -> Ordinal -> Property
prop_mul_right_monotonicity a b c = 0 < a && b < c ==> a * b < a * c

prop_finite :: Ordinal -> Bool
prop_finite a = (a < w) == finite a

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  r <- runTests 
  when r $ exitWith ExitSuccess
  exitWith (ExitFailure 1)
