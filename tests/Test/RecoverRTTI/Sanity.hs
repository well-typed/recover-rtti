{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.RecoverRTTI.Sanity (tests) where

import Data.SOP.BasicFunctors
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.QuickCheck.DepGen

import qualified Test.RecoverRTTI.QuickCheck.Sized as SG

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Sanity" [
     testProperty "typeSize"            prop_typeSize
   , testCase     "derivingVia"         test_derivingVia
   , testCase     "BoxAnythingToString" test_BoxAnythingToString
   ]

prop_typeSize :: Property
prop_typeSize =
    forAll (Blind <$> SG.run 10 arbitraryConcrete) $
      \(Blind (Some (DepGen classifier _))) ->
          counterexample ("classifier: " ++ show classifier)
        $ counterexample ("size: " ++ show (sizeConcrete classifier))
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
  BoxAnythingToString
-------------------------------------------------------------------------------}

data T3 f = T3 [f Any]

deriving instance Show a => Show (T3 (K a))

t3BadExample :: T3 I
t3BadExample = T3 [unsafeCoerce (1 :: Int), unsafeCoerce False]

t3GoodExample :: T3 (K BoxAnything)
t3GoodExample = T3 [K $ BoxAnything (1 :: Int), K $ BoxAnything False]

test_BoxAnythingToString :: Assertion
test_BoxAnythingToString = do
    assertBool "bad" $
      anythingToString t3BadExample /= "T3 [1,False]"
    assertEqual "good - show" "T3 [K 1,K False]" $
      show t3GoodExample
    assertEqual "good - anythingToString" "T3 [BoxAnything 1,BoxAnything False]" $
      anythingToString t3GoodExample