module Test.RecoverRTTI.Sanity (tests) where

import Debug.RecoverRTTI

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.QuickCheck.DepGen

import qualified Test.RecoverRTTI.QuickCheck.Sized as SG

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Sanity" [
     testProperty "typeSize" prop_typeSize
   ]

prop_typeSize :: Property
prop_typeSize =
    forAll (Blind <$> SG.run 10 arbitraryConcrete) $
      \(Blind (Some (DepGen classifier _))) ->
          counterexample ("classifier: " ++ show classifier)
        $ counterexample ("size: " ++ show (sizeConcrete classifier))
        $ sizeConcrete classifier <= 100
