-- | Verify that 'showAnything' produces same result as 'show'
module Test.RecoverRTTI.Show (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck hiding (classify)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Util

import Test.RecoverRTTI.Arbitrary

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Show" [
      testProperty "showAnything" prop_showAnything
    ]

-- | Compare " normal " 'show' against the " recovered " 'show'
prop_showAnything :: Some Value -> Property
prop_showAnything (Exists (Value _cc x)) =
      counterexample ("inferred: " ++ show (classify x))
    $ show x === showAnything x
