module Test.RecoverRTTI.Sanity (tests) where

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.SOP.BasicFunctors
import GHC.Exts (Any)
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI

import Test.RecoverRTTI.ConcreteClassifier.Arbitrary
import Test.RecoverRTTI.ConcreteClassifier.Size
import Test.RecoverRTTI.QuickCheck.DepGen
import Test.RecoverRTTI.QuickCheck.Sized qualified as SG

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Sanity" [
     testProperty "typeSize"            prop_typeSize
   , testCase     "derivingVia"         test_derivingVia

   , testGroup "Any" [
         testCase "listSimple"    test_Any_listSimple
       , testCase "listChar"      test_Any_listChar
       , testCase "hashMapSimple" test_Any_hashMapSimple
       , testCase "hashMapUnit"   test_Any_hashMapUnit
       ]
   ]

prop_typeSize :: Property
prop_typeSize =
    QC.forAll (QC.Blind <$> SG.run 10 arbitraryConcrete) $
      \(QC.Blind (Some (DepGen classifier _))) ->
          QC.counterexample ("classifier: " ++ show classifier)
        $ QC.counterexample ("size: " ++ show (sizeConcrete classifier))
        $ sizeConcrete classifier <= 100

{-------------------------------------------------------------------------------
  Deriving-via support
-------------------------------------------------------------------------------}

data T1 = T1 Int Bool

data T2 = T2 T1
  deriving Show via AnythingToString T2

test_derivingVia :: Assertion
test_derivingVia = assertEqual "" "T2 (T1 1 True)" $ show (T2 (T1 1 True))

{-------------------------------------------------------------------------------
  Unsafe heterogenous datastructures
-------------------------------------------------------------------------------}

-- | Simple hererogeneous lists (without 'Char')
test_Any_listSimple :: Assertion
test_Any_listSimple =
    assertEqual "" "[1,False]" $ anythingToString testValue
  where
    testValue :: [I Any]
    testValue = [
          unsafeCoerce (1 :: Int)
        , unsafeCoerce False
        ]

-- | Hererogeneous lists that start with a 'Char'
--
-- This is particularly unpleasant, as these might be mistaken for 'String's.
test_Any_listChar :: Assertion
test_Any_listChar =
    assertEqual "" "['a',1,False]" $ anythingToString testValue
  where
    testValue :: [I Any]
    testValue = [
          unsafeCoerce 'a'
        , unsafeCoerce (1 :: Int)
        , unsafeCoerce False
        ]

test_Any_hashMapSimple :: Assertion
test_Any_hashMapSimple =
    assertEqual "" "fromList [(1,'a'),(2,False)]" $ anythingToString testValue
  where
    testValue :: HashMap Int Any
    testValue = HashMap.fromList [
          (1, unsafeCoerce 'a')
        , (2, unsafeCoerce False)
        ]

-- | HashMap with () as the first value will be mistaken for a HashSet
test_Any_hashMapUnit :: Assertion
test_Any_hashMapUnit =
    assertEqual "" "fromList [0,1,2]" $ anythingToString testValue
  where
    testValue :: HashMap Int Any
    testValue = HashMap.fromList [
          (0, unsafeCoerce ())
        , (1, unsafeCoerce 'a')
        , (2, unsafeCoerce False)
        ]
