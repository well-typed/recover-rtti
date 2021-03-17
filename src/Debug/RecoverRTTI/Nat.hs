{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

-- | Inductive type-level natural numbers
module Debug.RecoverRTTI.Nat (
    -- * Type-level natural numbers
    Nat(..)
  , SNat(..)
  , KnownNat(..)
  , natVal
    -- * Type level functions computing natural numbers
  , Length
  ) where

{-------------------------------------------------------------------------------
  Natural numbers
-------------------------------------------------------------------------------}

-- | Natural numbers
--
-- Intended to be used lifted to the type level; unlike @ghc@'s type level
-- natural numbers, these are inductive.
data Nat = Z | S Nat

-- | Singleton for 'Nat'
data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

class KnownNat (n :: Nat) where
  singNat :: SNat n

instance               KnownNat 'Z     where singNat = SZ
instance KnownNat n => KnownNat ('S n) where singNat = SS singNat

natVal :: forall n proxy. KnownNat n => proxy n -> Int
natVal _ = go (singNat :: SNat n)
  where
    go :: forall m. SNat m -> Int
    go SZ     = 0
    go (SS n) = go n + 1

{-------------------------------------------------------------------------------
  Type-level functions computing natural numbers
-------------------------------------------------------------------------------}

type family Length (xs :: [k]) :: Nat where
  Length '[]       = 'Z
  Length (_ ': xs) = 'S (Length xs)
