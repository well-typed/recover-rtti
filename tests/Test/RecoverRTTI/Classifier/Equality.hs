-- | Equality orphan instances
module Test.RecoverRTTI.Classifier.Equality () where

import Data.Function (on)

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array

import Test.RecoverRTTI.Prim ()

instance Eq a => Eq (HashMap.Array a) where
  (==) = (==) `on` HashMap.Array.toList
