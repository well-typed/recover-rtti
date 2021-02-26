{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.RecoverRTTI.Show (tests) where

import Control.DeepSeq

import Test.Tasty
import Test.Tasty.QuickCheck

import Debug.RecoverRTTI

import Test.RecoverRTTI.Arbitrary

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Show" [
      testProperty "constants"   prop_constants
    , testProperty "arbitraryNF" prop_arbitraryNF
    ]

-- | Test using manually specified examples
--
-- For " normal " code it doesn't matter if something is generated or not,
-- but their on-heap representation may be different, and this may effect the
-- RTTI recovery.
prop_constants :: Property
prop_constants = withMaxSuccess 1 $ conjoin [
      compareShow $ Value CC_Unit          ()
    , compareShow $ Value CC_Bool          True
    , compareShow $ Value CC_Bool          False
    , compareShow $ Value CC_Int           0
    , compareShow $ Value CC_Int           1_000_000
    , compareShow $ Value CC_Char          'a'
    , compareShow $ Value CC_Double        1.25
    , compareShow $ Value (CC_List CC_Int) []
    , compareShow $ Value (CC_List CC_Int) [1, 2, 3]
    , compareShow $ Value CC_UD_NR         (NR1 1234)
    , compareShow $ Value CC_UD_NR         (NR2 'a' True)
    ]
  where
    _checkAllCases :: ConcreteClassifier a -> ()
    _checkAllCases = \case
        CC_Unit   -> ()
        CC_Bool   -> ()
        CC_Int    -> ()
        CC_Char   -> ()
        CC_Double -> ()
        CC_List _ -> ()
        CC_UD_NR  -> ()

-- | Test using arbitrary values, but force them to NF before showing them.
prop_arbitraryNF :: Value -> Property
prop_arbitraryNF v@(Value _ x) = rnf x `seq` compareShow v

-- | Compare " normal " 'show' against the " recovered " 'show'
--
-- The tests in this module differ only in how the produce the 'Value's.
compareShow :: Value -> Property
compareShow (Value _ x) = show x === showAnything x
