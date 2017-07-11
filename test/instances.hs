{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Hack for bug in older Cabal versions
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif

import Deriving

import Control.Lens
import Control.Lens.Action
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Data.Lens
import Data.Fixed (Fixed, E1)
import Data.List
import Data.SafeCopy.Store
import Data.Store (decodeWith, decodeExWith)
import Data.Time (ZonedTime(..))
import Data.Tree (Tree)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Fixed, (===))
import Test.Tasty.HUnit

#if ! MIN_VERSION_QuickCheck(2,9,0)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) =>
         Arbitrary (a,b,c,d,e,f) where
   arbitrary = (,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*>
                           arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g) =>
         Arbitrary (a,b,c,d,e,f,g) where
   arbitrary = (,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*>
                            arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#endif

#if ! MIN_VERSION_QuickCheck(2,8,2)
instance (Arbitrary a) => Arbitrary (V.Vector a) where
   arbitrary = V.fromList <$> arbitrary

instance (Arbitrary a, VP.Prim a) => Arbitrary (VP.Vector a) where
   arbitrary = VP.fromList <$> arbitrary

instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
   arbitrary = VS.fromList <$> arbitrary

instance (Arbitrary a, VU.Unbox a) => Arbitrary (VU.Vector a) where
   arbitrary = VU.fromList <$> arbitrary
#endif

deriving instance (Arbitrary a) => Arbitrary (Prim a)
deriving instance (Eq a) => Eq (Prim a)
deriving instance (Show a) => Show (Prim a)

deriving instance Eq ZonedTime
#if ! MIN_VERSION_time(1,6,0)
deriving instance Show UniversalTime
#endif

data Example = Example {
    exampleField1 :: String
  , exampleField2 :: Bool
  } deriving (Show, Eq)

deriveSafeCopy 1 'base ''Example

instance Arbitrary Example where
  arbitrary = Example <$> arbitrary <*> arbitrary

-- | Equality on the 'Right' value, showing the unequal value on failure;
-- or explicit failure using the 'Left' message without equality testing.
(===) :: (Eq a, Show a) => Either PeekException a -> a -> Property
Left  e === _ = counterexample (show e) False
Right a === b = counterexample (show a) $ a == b

-- | An instance for 'SafeCopy' makes a type isomorphic to a bytestring
-- serialization, which is to say that @decode . encode = id@, i.e.
-- @decode@ is the inverse of @encode@ if we ignore bottom.
prop_inverse :: (SafeCopy a, Arbitrary a, Eq a, Show a) => a -> Property
prop_inverse a = (decode . encode) a === a where
    encode = runEncode . safePut
    decode = decodeWith safeGet

-- | Test the 'prop_inverse' property against all 'SafeCopy' instances
-- (that also satisfy the rest of the constraints) defaulting any type
-- variables to 'Int'.
do let a = conT ''Int

   -- types we skip because the Int defaulting doesn't type check
   excluded <- sequence
      [ [t| Fixed $a |]
      ]

   -- instead we include these hand-defaulted types
   included <- sequence
      [ [t| Fixed E1 |]
      , [t| Example |]
      ]

   -- types whose samples grow exponentially and need a lower maxSize
   downsized <- sequence
      [ [t| Array $a $a |]
      , [t| UArray $a $a |]
      , [t| Tree $a |]
      ]

   safecopy <- reify ''SafeCopy
   preds <- 'prop_inverse ^!! act reify . (template :: Traversal' Info Pred)
#if !MIN_VERSION_template_haskell(2,10,0)
   classes <- mapM reify [ name | ClassP name _ <- preds ]
#else
--   print preds

   classes <-
         case preds of
           [ForallT _ cxt' _] ->
              mapM reify [ name | AppT (ConT name) _ <- cxt' ]
           _ -> error "FIXME: fix this code to handle this case."
--   classes <- mapM reify [ ]
#endif
   def <- a

#if MIN_VERSION_template_haskell(2,11,0)
   let instances (ClassI _ decs) = [ typ | InstanceD _ _ (AppT _ typ) _ <- decs ]
#else
   let instances (ClassI _ decs) = [ typ | InstanceD _ (AppT _ typ) _ <- decs ]
#endif
       instances _ = []
       types = map instances classes

       defaulting (VarT _) = def
       defaulting t = t
       defaulted = transformOn (traverse.traverse) defaulting types
       wanted = transformOn traverse defaulting $ instances safecopy

       common = foldl1 intersect defaulted
       untested = wanted \\ common
       exclusive = filter (`notElem` excluded) common

       downsize typ | typ `elem` downsized = [| mapSize (`div` 5) |]
                    | otherwise            = [| id |]

       unqualifying (Name occ _) = Name occ NameS
       name = pprint . transformOnOf template template unqualifying

       prop typ =
           [| testProperty $(litE . stringL $ name typ)
               ($(downsize typ) (prop_inverse :: $(return typ) -> Property)) |]

       props = listE . map prop

#if !MIN_VERSION_template_haskell(2,8,0)
       -- 'report' throws warnings in template-haskell-2.8.0.0
       reportWarning = report False
#endif

   mapM_ (\typ -> reportWarning $ "not tested: " ++ name typ) untested

   [d| inversions :: [TestTree]
       inversions = $(props included) ++ $(props exclusive) |]

data AV0 = AV0 Int deriving (Eq, Show)
deriveSafeCopy 1 'base ''AV0

data AV1 = AV1 Int Int deriving (Eq, Show)
deriveSafeCopy 2 'extension ''AV1

instance Migrate AV1 where
  type MigrateFrom AV1 = AV0
  migrate (AV0 n) = AV1 n 0

data BV0 = BV0 {
  bfield1 :: Int
} deriving (Eq, Show)
deriveSafeCopy 1 'base ''BV0

data BV1 = BV1 {
  bfield2 :: Int
, bfield3 :: Int
} deriving (Eq, Show)
deriveSafeCopy 2 'extension ''BV1

instance Migrate BV1 where
  type MigrateFrom BV1 = BV0
  migrate (BV0 n) = BV1 n 0

migrationTest :: [TestTree]
migrationTest = [
    testCase "Extension test" $ let
      bs = runEncode $ safePut $ AV0 42
      v2 = decodeExWith safeGet bs
      in v2 @?= AV1 42 0
  , testCase "Extension+rename test" $ let
      bs = runEncode $ safePut $ BV0 42
      v2 = decodeExWith safeGet bs
      in v2 @?= BV1 42 0
  ]

main :: IO ()
main = defaultMain $ testGroup "SafeCopy instances"
    [ testGroup "decode is the inverse of encode" inversions
    , testGroup "can decode with migration" migrationTest
    ]
