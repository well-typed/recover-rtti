module Test.RecoverRTTI.Orphans () where

import Data.Function (on)

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array

import Debug.RecoverRTTI

-- | Degenerate 'Eq' instance for functions that always says 'True'
--
-- When we compare values up to the coercion returned by 'reclassify', we need
-- an 'Eq' instance. We can't compare functions in any meaningful way though,
-- and so we just return 'True' here no matter what.
--
-- This is an orphan defined in the test suite only, so that users of the
-- library don't have acccess to this (misleading) instance.
instance Eq SomeFun where
  _ == _ = True

instance Eq a => Eq (HashMap.Array a) where
  (==) = (==) `on` HashMap.Array.toList
