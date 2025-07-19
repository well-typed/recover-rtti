module Main (main) where

import Test.Tasty

import Test.RecoverRTTI.Sanity   qualified
import Test.RecoverRTTI.Classify qualified
import Test.RecoverRTTI.Show     qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "RecoverRttiTests" [
      Test.RecoverRTTI.Sanity.tests
    , Test.RecoverRTTI.Classify.tests
    , Test.RecoverRTTI.Show.tests
    ]
