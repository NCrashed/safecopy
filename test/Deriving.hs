{-# LANGUAGE TemplateHaskell #-}
-- | Test for TH deriving
module Deriving where

import Data.SafeCopy.Store
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data A = A1 | A2 | A3
  deriving (Bounded, Enum, Show, Eq)

instance Arbitrary A where
  arbitrary = arbitraryBoundedEnum

deriveSafeCopy 1 'base ''A

data B = B1 | B2 Bool | B3 A A
  deriving (Show, Eq)

instance Arbitrary B where
  arbitrary = do
    n <- choose (0, 2 :: Int)
    case n of
      0 -> pure B1
      1 -> B2 <$> arbitrary
      2 -> B3 <$> arbitrary <*> arbitrary
      _ -> pure B1

deriveSafeCopy 1 'base ''B
