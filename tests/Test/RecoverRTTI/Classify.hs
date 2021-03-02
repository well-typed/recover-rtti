{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Verify we infer the right classifier
module Test.RecoverRTTI.Classify (tests) where

import Control.DeepSeq
import Data.Type.Equality

import Test.Tasty
import Test.Tasty.QuickCheck hiding (classify, NonEmpty)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Util

import Test.RecoverRTTI.Arbitrary
import Test.RecoverRTTI.Staged

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Classify" [
      testProperty "constants"   prop_constants
    , testProperty "arbitraryNF" prop_arbitraryNF
    , testProperty "arbitrary"   prop_arbitrary
    ]

-- | Test using manually specified examples
--
-- For " normal " code it doesn't matter if something is generated or not,
-- but their on-heap representation may be different, and this may effect the
-- RTTI recovery.
prop_constants :: Property
prop_constants = withMaxSuccess 1 $ conjoin [
      -- Primitive types

      compareClassifier $ Value CC_Bool     True
    , compareClassifier $ Value CC_Bool     False
    , compareClassifier $ Value CC_Char     'a'
    , compareClassifier $ Value CC_Double   1.25
    , compareClassifier $ Value CC_Float    1.25
    , compareClassifier $ Value CC_Int      1234
    , compareClassifier $ Value CC_Int      (-1234)
    , compareClassifier $ Value CC_Int8     123
    , compareClassifier $ Value CC_Int16    1234
    , compareClassifier $ Value CC_Int32    1234
    , compareClassifier $ Value CC_Int64    1234
    , compareClassifier $ Value CC_Ordering LT
    , compareClassifier $ Value CC_Ordering GT
    , compareClassifier $ Value CC_Ordering EQ
    , compareClassifier $ Value CC_Unit     ()
    , compareClassifier $ Value CC_Word     1234
    , compareClassifier $ Value CC_Word8    123
    , compareClassifier $ Value CC_Word16   134
    , compareClassifier $ Value CC_Word32   1234
    , compareClassifier $ Value CC_Word64   1234

      -- Compound

    , compareClassifier $ Value (CC_List Empty) []
    , compareClassifier $ Value (CC_List (NonEmpty CC_Int)) [1, 2, 3]

      -- User defined

    , compareClassifier $ Value (CC_User_NonRec Empty)              (NR1 1234)
    , compareClassifier $ Value (CC_User_NonRec (NonEmpty CC_Char)) (NR2 'a' True)
    , compareClassifier $ Value (CC_User_Rec    Empty)              RNil
    , compareClassifier $ Value (CC_User_Rec    (NonEmpty CC_Char)) (RCons 'a' RNil)
    ]
  where
    _checkAllCases :: ConcreteClassifier a -> ()
    _checkAllCases = \case
        -- Primitive types

        CC_Bool     -> ()
        CC_Char     -> ()
        CC_Double   -> ()
        CC_Float    -> ()
        CC_Int      -> ()
        CC_Int8     -> ()
        CC_Int16    -> ()
        CC_Int32    -> ()
        CC_Int64    -> ()
        CC_Ordering -> ()
        CC_Unit     -> ()
        CC_Word     -> ()
        CC_Word8    -> ()
        CC_Word16   -> ()
        CC_Word32   -> ()
        CC_Word64   -> ()

        -- Compound

        CC_List _ -> ()

        -- User-defined

        CC_User_NonRec _ -> ()
        CC_User_Rec    _ -> ()

-- | Test using arbitrary values, but force them to NF before classifying them.
prop_arbitraryNF :: Some Value -> Property
prop_arbitraryNF (Exists v@(Value _ x)) = rnf x `seq` compareClassifier v

-- | Test using arbitrary values (NF or not)
prop_arbitrary :: Some Value -> Property
prop_arbitrary (Exists v) = compareClassifier v

-- | Compare given to inferred classifier
--
-- The tests in this module differ only in how the produce the 'Value's.
compareClassifier :: Value a -> Property
compareClassifier = \(Value cc x) ->
      counterexample ("Generated classifier: " ++ show cc)
    $ case reclassify (classified x) of
        Nothing  ->
          counterexample "Failed to reclassify" $ property False
        Just (Reclassified cc' f) ->
          case sameConcreteClassifier cc cc' of
            Nothing ->
                counterexample ("Inferred different classifier: " ++ show cc')
              $ property False
            Just Refl ->
              x === f x
