{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnboxedTuples     #-}

-- | Just some examples of user-defined types
module Test.RecoverRTTI.UserDefined (
    NonRecursive(..)
  , Recursive(..)
  , recursiveFromList
  , ContainsUnlifted -- opaque
  , exampleContainsUnlifted
  ) where

import GHC.Generics
import GHC.IO
import GHC.Prim

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

{-------------------------------------------------------------------------------
  Example of a type with an unlifted value

  Most instances here don't really make much sense; this is just here to
  verify we don't crash when coming across unlifted values
-------------------------------------------------------------------------------}

-- | Example of a user-defined type containing something unlifted
data ContainsUnlifted a = NothingHere | ContainsUnlifted (MutableArray# RealWorld Int) a
  deriving (Functor, Foldable, Traversable)

-- We can't derive a Generic instance, we must produce just enough info so that
-- we can do the right type casts
instance Generic (ContainsUnlifted a) where
  type Rep (ContainsUnlifted a) = Rep_ContainsUnlifted

  from = error "'from' not defined for ContainsUnlifted"
  to   = error "'to' not defined for ContainsUnlifted"

-- We don't bother specifiying the arguments to the constructors
type Rep_ContainsUnlifted =
    M1 D ('MetaData "ContainsUnlifted" "Test.RecoverRTTI.UserDefined" "main" 'False)
     (     M1 C ('MetaCons "NothingHere" 'PrefixI 'False) U1
       :+:
           M1 C ('MetaCons "ContainsUnlifted" 'PrefixI 'False) U1
     )

instance Show a => Show (ContainsUnlifted a) where
  showsPrec _ NothingHere =
        showString "NothingHere"
  showsPrec p (ContainsUnlifted _ x) = showParen (p >= 11) $
        showString "ContainsUnlifted "
      . showsPrec 11 x

instance Eq (ContainsUnlifted a) where
  _ == _ = True

exampleContainsUnlifted :: ContainsUnlifted ()
{-# NOINLINE exampleContainsUnlifted #-}
exampleContainsUnlifted = unsafePerformIO $ IO $ \world ->
    let !(# world', arr #) = newArray# 5# 0 world
    in (# world', ContainsUnlifted arr () #)
