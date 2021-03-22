{-# LANGUAGE RankNTypes #-}

-- | Equality
--
-- This is not defined in the main lib, because this depends on orphan 'Eq'
-- instances, some of which are a bit dubious (especially some of the ones
-- defined in "Test.RecoverRTTI.Prim").
module Test.RecoverRTTI.Classifier.Equality (canCompareClassified_) where

import Data.Function (on)
import Data.SOP.Dict

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array

import Debug.RecoverRTTI

import Test.RecoverRTTI.Prim ()

{-------------------------------------------------------------------------------
  All types recognized by the library have Eq instances
-------------------------------------------------------------------------------}

canCompareClassified_ :: forall o.
     (forall a. o a -> Dict Eq a)
  -> (forall a. Classifier_ o a -> Dict Eq a)
canCompareClassified_ = classifiedSatisfies

{-------------------------------------------------------------------------------
  Orphan Eq instances
-------------------------------------------------------------------------------}

instance Eq a => Eq (HashMap.Array a) where
  (==) = (==) `on` HashMap.Array.toList
