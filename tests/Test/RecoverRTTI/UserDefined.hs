{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Just some examples of user-defined types
module Test.RecoverRTTI.UserDefined (
    NonRecursive(..)
  , Recursive(..)
  , recursiveFromList
  ) where

import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  User-defined datatypes
-------------------------------------------------------------------------------}

-- | Example of a non-recursive user-defined type
data NonRecursive a = NR1 Int | NR2 a Bool
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Example of a recursive user-defined type
data Recursive a = RNil | RCons a (Recursive a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

recursiveFromList :: [a] -> Recursive a
recursiveFromList = foldr RCons RNil
