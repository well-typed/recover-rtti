{-# LANGUAGE NumericUnderscores #-}

-- | Verify that 'anythingToString' produces same result as 'show'
module Test.RecoverRTTI.Show (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck hiding (classify)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Util

import Test.RecoverRTTI.Arbitrary ()
import Test.RecoverRTTI.ConcreteClassifier

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Show" [
      testProperty "showGenerated"    prop_showGenerated
    , testProperty "anythingToString" prop_anythingToString
    ]

-- | Check that the generated value is showable
--
-- This is a sanity check on the generator.
prop_showGenerated :: Some Value -> Property
prop_showGenerated (Some (Value _cc x)) =
     counterexample ("length: " ++ show (length (show x)))
   $ property (length (show x) < 1_000_000)

-- | Compare " normal " 'show' against the " recovered " 'show'
prop_anythingToString :: Some Value -> Property
prop_anythingToString (Some (Value _cc x)) =
      counterexample ("inferred: " ++ show (classify x))
    $ within 1_000_000
    $ show x === anythingToString x
