{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug.RecoverRTTI.Util.TypeLevel (
    -- * Singletons
    Sing(..)
  , SingI(..)
  , DecidableEquality(..)
    -- ** Natural numbers
  , Nat(..)
  , knownNat
  , Length
    -- * General purpose type level functions
  , Or
  , Equal
  , Elem
  , Assert
    -- * Type-level membership check
  , IsElem(..)
  , checkIsElem
    -- * Phantom type parameters
  , Phantom(..)
  , Poly(..)
  , maybePoly
  ) where

import Data.Kind
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits (ErrorMessage, Symbol, KnownSymbol, TypeError, sameSymbol)

{-------------------------------------------------------------------------------
  Singletons
-------------------------------------------------------------------------------}

data family Sing :: k -> Type

class SingI (a :: k) where
  sing :: Sing a

class DecidableEquality k where
  decideEquality :: Sing (a :: k) -> Sing (b :: k) -> Maybe (a :~: b)

{-------------------------------------------------------------------------------
  For kind 'Type', Sing is just a proxy
-------------------------------------------------------------------------------}

data instance Sing (a :: Type) where
  SProxy :: Sing (a :: Type)

instance SingI (a :: Type) where
  sing = SProxy

{-------------------------------------------------------------------------------
  Natural numbers

  Unlike @ghc@'s, these are inductively defined.
-------------------------------------------------------------------------------}

data Nat = Z | S Nat

data instance Sing (n :: Nat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

instance            SingI 'Z     where sing = SZ
instance SingI n => SingI ('S n) where sing = SS sing

knownNat :: Sing (n :: Nat) -> Int
knownNat SZ     = 0
knownNat (SS n) = knownNat n + 1

type family Length (xs :: [k]) :: Nat where
  Length '[]       = 'Z
  Length (_ ': xs) = 'S (Length xs)

{-------------------------------------------------------------------------------
  Singleton instance for type-level symbols
-------------------------------------------------------------------------------}

data instance Sing (n :: Symbol) where
  SSymbol :: KnownSymbol n => Sing n

instance KnownSymbol n => SingI (n :: Symbol) where
  sing = SSymbol

instance DecidableEquality Symbol where
  decideEquality SSymbol SSymbol = sameSymbol Proxy Proxy

{-------------------------------------------------------------------------------
  Singleton instance for lists
-------------------------------------------------------------------------------}

data instance Sing (xs :: [k]) where
  SN :: Sing '[]
  SC :: Sing x -> Sing xs -> Sing (x ': xs)

instance                        SingI '[]       where sing = SN
instance (SingI x, SingI xs) => SingI (x ': xs) where sing = SC sing sing

{-------------------------------------------------------------------------------
  General purpose type level functions
-------------------------------------------------------------------------------}

type family Or (a :: Bool) (b :: Bool) where
  Or 'True b     = 'True
  Or a     'True = 'True
  Or _     _     = 'False

type family Equal (x :: k) (y :: k) where
  Equal x x = 'True
  Equal x y = 'False

type family Elem (x :: k) (xs :: [k]) where
  Elem x '[]       = 'False
  Elem x (y ': ys) = Or (Equal x y) (Elem x ys)

-- | Assert type-level predicate
--
-- We cannot define this in terms of a more general @If@ construct, because
-- @ghc@'s type-level language has an undefined reduction order and so we get
-- no short-circuiting.
type family Assert (b :: Bool) (err :: ErrorMessage) :: Constraint where
  Assert 'True  err = ()
  Assert 'False err = TypeError err

{-------------------------------------------------------------------------------
  Decidable equality gives a decidable membership check
-------------------------------------------------------------------------------}

data IsElem (x :: k) (xs :: [k]) where
  IsElem :: Elem x xs ~ 'True => IsElem x xs

shiftIsElem :: IsElem x ys -> IsElem x (y ': ys)
shiftIsElem IsElem = IsElem

checkIsElem ::
     DecidableEquality k
  => Sing (x :: k) -> Sing (xs :: [k]) -> Maybe (IsElem x xs)
checkIsElem _ SN         = Nothing
checkIsElem x (SC y ys) = case decideEquality x y of
                            Just Refl -> Just IsElem
                            Nothing   -> shiftIsElem <$> checkIsElem x ys

{-------------------------------------------------------------------------------
  Phantom type parameters
-------------------------------------------------------------------------------}

-- | Functors with phantom arguments
class Phantom (f :: k -> Type) where
  -- | Similar to 'Data.Functor.Contravariant.phantom', but without requiring
  -- 'Functor' or 'Contravariant'
  phantom :: forall a b. f a -> f b

data Poly (f :: k -> Type) = Poly (forall (a :: k). f a)

-- | Commute @Maybe@ and @forall@
maybePoly :: Phantom f => Maybe (f a) -> Maybe (Poly f)
maybePoly = fmap (\v -> Poly (phantom v))
