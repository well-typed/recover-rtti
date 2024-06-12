{-# OPTIONS_GHC -Wno-orphans #-}

-- | Equality orphan instances
module Test.RecoverRTTI.Classifier.Equality () where

import Data.Function (on)

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array

import Debug.RecoverRTTI

{-------------------------------------------------------------------------------
  Reasonable instances
-------------------------------------------------------------------------------}

instance Eq a => Eq (HashMap.Array a) where
  (==) = (==) `on` HashMap.Array.toList

{-------------------------------------------------------------------------------
  Degenerate instances

  It is (obviously!) important that these are available in the test suite only.
-------------------------------------------------------------------------------}

instance Eq SomeFun where
  _ == _ = True

instance Eq SomePrimArrayM where
  _ == _ = True

instance Eq SomeStorableVector where
  _ == _ = True

instance Eq SomeStorableVectorM where
  _ == _ = True

instance Eq SomePrimitiveVector where
  _ == _ = True

instance Eq SomePrimitiveVectorM where
  _ == _ = True

instance Eq SomeMutableByteArray where
  _ == _ = True
