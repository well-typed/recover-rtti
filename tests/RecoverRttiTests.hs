module Main (main) where

import Test.Tasty

import qualified Test.RecoverRTTI.Sanity
import qualified Test.RecoverRTTI.Classify
import qualified Test.RecoverRTTI.Show

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "RecoverRttiTests" [
      Test.RecoverRTTI.Sanity.tests
    , Test.RecoverRTTI.Classify.tests
    , Test.RecoverRTTI.Show.tests
    ]
