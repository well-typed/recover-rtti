{-# LANGUAGE NamedFieldPuns #-}

module Test.RecoverRTTI.Sanity (tests) where

import Debug.RecoverRTTI

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.RecoverRTTI.Arbitrary
import Test.RecoverRTTI.ConcreteClassifier

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Sanity" [
     testProperty "typeSize" prop_typeSize
   ]

prop_typeSize :: Property
prop_typeSize =
    forAll (Blind <$> arbitraryClassifiedGen 10) $
      \(Blind (Some ClassifiedGen{genClassifier})) ->
          counterexample ("classifier: " ++ show genClassifier)
        $ counterexample ("size: " ++ show (classifierSize genClassifier))
        $ classifierSize genClassifier <= 100
