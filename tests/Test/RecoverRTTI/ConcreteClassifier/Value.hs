module Test.RecoverRTTI.ConcreteClassifier.Value (
    Value(..)
  ) where

import Test.QuickCheck (Arbitrary(..))

import Debug.RecoverRTTI

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.ConcreteClassifier.Arbitrary
import Test.RecoverRTTI.QuickCheck.DepGen
import Test.RecoverRTTI.QuickCheck.Sized qualified as SG

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Like 'Classified', but using 'Concrete'
--
-- For convenience, we also include some constraints here, even though they
-- are in fact derivable from the classifier
data Value a where
   Value :: (Show a, Eq a) => Concrete a -> a -> Value a

deriving instance Show (Value a)
deriving instance Show (Some Value)

instance Arbitrary (Some Value) where
  arbitrary = do
      -- We don't want to generate large classifiers
      Some (DepGen cc gen) <- SG.run 10 arbitraryConcrete

      -- For the values however we want to be able to generate larger trees
      Some . Value cc <$> SG.run 1000 gen

