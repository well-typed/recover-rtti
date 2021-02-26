{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

-- So we can compare the instances ghc derives for our "user-defined types"
-- to what we'd do with the recovered type information
{-# OPTIONS_GHC -ddump-deriv -ddump-to-file #-}

module Test.RecoverRTTI.Arbitrary (
    ConcreteClassifier(..)
  , Value(..)
    -- * Examples of user-defined types
  , NonRecursive(..)
  , Recursive(..)
  ) where

import Control.DeepSeq
import Data.Kind
import GHC.Generics (Generic)

import Test.QuickCheck

import Debug.RecoverRTTI

{-------------------------------------------------------------------------------
  Values of arbitrary types
-------------------------------------------------------------------------------}

-- | Like 'Classifier', but with no guess-work and concrete types
data ConcreteClassifier (a :: Type) :: Type where
    -- Primitive types

    CC_Unit   :: ConcreteClassifier ()
    CC_Bool   :: ConcreteClassifier Bool
    CC_Int    :: ConcreteClassifier Int
    CC_Char   :: ConcreteClassifier Char
    CC_Double :: ConcreteClassifier Double

    -- Compound

    CC_List   :: ConcreteClassifier a -> ConcreteClassifier [a]

    -- User-defined

    CC_UD_NR  :: ConcreteClassifier (NonRecursive Char)

-- | Like 'Classified', but using 'ConcreteClassifier' and with constraints
data Value where
    Value :: (Show a, NFData a) => ConcreteClassifier a -> a -> Value

deriving instance Show (ConcreteClassifier a)
deriving instance Show Value

arbitraryClassifier ::
     (forall a.
             (Arbitrary a, Show a, NFData a)
          => ConcreteClassifier a -> Gen r
        )
  -> Gen r
arbitraryClassifier k = oneof [
      k CC_Unit
    , k CC_Bool
    , k CC_Int
    , k CC_Char
    , k CC_Double
   --, arbitraryClassifier (k . CC_List)
    , k CC_UD_NR
    ]
  where
    _checkAllCases :: Classifier a -> ()
    _checkAllCases = \case
        C_Void     -> () -- We cannot generate a value for Void, of course
        C_Unit     -> ()
        C_Bool     -> ()
        C_Int      -> ()
        C_Char     -> ()
        C_Double   -> ()
        C_List{}   -> ()
        C_Custom{} -> ()

instance Arbitrary Value where
  arbitrary = arbitraryClassifier $ \cc -> Value cc <$> arbitrary

{-------------------------------------------------------------------------------
  User-defined datatypes
-------------------------------------------------------------------------------}

-- | Example of a non-recursive user-defined type
data NonRecursive a = NR1 Int | NR2 a Bool
  deriving (Show, Generic, NFData)

-- | Example of a recursive user-defined type
data Recursive a = RNil | RCons a (Recursive a)

instance Arbitrary a => Arbitrary (NonRecursive a) where
  arbitrary = oneof [
        NR1 <$> arbitrary
      , NR2 <$> arbitrary <*> arbitrary
      ]

instance Arbitrary a => Arbitrary (Recursive a) where
  arbitrary = conv <$> arbitrary
    where
      conv :: [a] -> Recursive a
      conv = foldr RCons RNil
