{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Just some examples of user-defined types
module Test.RecoverRTTI.UserDefined (
    SimpleType(..)
  , NonRecursive(..)
  , Recursive(..)
  , recursiveFromList
  , ContainsUnlifted -- opaque
  , exampleContainsUnlifted
  , ConstrsOf(..)
  , ClassifyUser(..)
  ) where

import Data.Kind
import Data.Proxy
import GHC.Exts (RealWorld, MutableArray#, newArray#)
import GHC.Generics
import GHC.IO (IO(..))
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck

import Debug.RecoverRTTI

{-------------------------------------------------------------------------------
  User-defined datatypes
-------------------------------------------------------------------------------}

-- | Example of a simple monomorphic user-defined type
data SimpleType = SimpleA | SimpleB
  deriving (Show, Eq, Generic)

-- | Example of a non-recursive user-defined type
data NonRecursive a = NR1 Int | NR2 Bool a
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Example of a recursive user-defined type
data Recursive a = RNil | RCons a (Recursive a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

recursiveFromList :: [a] -> Recursive a
recursiveFromList = foldr RCons RNil

{-------------------------------------------------------------------------------
  Example of a type with an unlifted value

  Most instances here don't really make much sense; this is just here to
  verify we don't crash when coming across unlifted values
-------------------------------------------------------------------------------}

-- | Example of a user-defined type containing something unlifted
data ContainsUnlifted = ContainsUnlifted (MutableArray# RealWorld Int) Bool

instance Show ContainsUnlifted where
  showsPrec p (ContainsUnlifted _ x) = showParen (p >= 11) $
        showString "ContainsUnlifted "
      . showsPrec 11 x

instance Eq ContainsUnlifted where
  _ == _ = True

exampleContainsUnlifted :: ContainsUnlifted
{-# NOINLINE exampleContainsUnlifted #-}
exampleContainsUnlifted = unsafePerformIO $ IO $ \world ->
    let !(# world', arr #) = newArray# 5# 0 world
    in (# world', ContainsUnlifted arr True #)

{-------------------------------------------------------------------------------
  ConstrsOf
-------------------------------------------------------------------------------}

-- | Constructors of this type
--
-- This could be defined in terms of generics, but this is kind polymorphic.
-- Used for testing only.
class ConstrsOf (f :: k) where
  constrsOf :: Proxy f -> [String]

instance ConstrsOf SimpleType       where constrsOf _ = ["SimpleA", "SimpleB"]
instance ConstrsOf Recursive        where constrsOf _ = ["RNil", "RCons"]
instance ConstrsOf NonRecursive     where constrsOf _ = ["NR1", "NR2"]
instance ConstrsOf ContainsUnlifted where constrsOf _ = ["NothingHere", "ContainsUnlifted"]

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary SimpleType where
  arbitrary = elements [SimpleA, SimpleB]

instance Arbitrary ContainsUnlifted where
  arbitrary = return exampleContainsUnlifted

{-------------------------------------------------------------------------------
  Classifier
-------------------------------------------------------------------------------}

data ClassifyUser (a :: Type) where
  C_Simple   :: ClassifyUser SimpleType
  C_NonRec   :: ClassifyUser (NonRecursive Deferred)
  C_Rec      :: ClassifyUser (Recursive    Deferred)
  C_Unlifted :: ClassifyUser ContainsUnlifted

deriving stock instance Show (ClassifyUser a)