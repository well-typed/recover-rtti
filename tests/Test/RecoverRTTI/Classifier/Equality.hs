-- | Equality orphan instances
module Test.RecoverRTTI.Classifier.Equality () where

import Data.Function (on)

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array
import qualified GHC.Arr                     as GHC.Arr

import Debug.RecoverRTTI

import Test.RecoverRTTI.Prim ()

instance Eq a => Eq (HashMap.Array a) where
  (==) = (==) `on` HashMap.Array.toList

instance (Eq i, Eq e) => Eq (GhcArray i e) where
  (==) = (==) `on` (\(GhcArray a) -> (GHC.Arr.bounds a, GHC.Arr.elems a))
