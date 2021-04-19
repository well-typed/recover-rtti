{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Support for reclassification
module Debug.RecoverRTTI.Reclassify (
    Reclassified(..)
  , reclassify_
  , distribReclassified
  , FromUsr(..)
  , coerceFromUsr
  ) where

import Data.Kind
import Data.SOP hiding (NS(..))
import Data.Void
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Wrappers

-- | Reclassified values
--
-- Reclassification can be done by user code which want to take advantage of
-- the classification infrastructure for @recover-rtti@ but add some additional
-- classification for domain-specific types known only to that client code.
--
-- When we reclassify a value, a value that might previously be classified as
-- @UserDefined@ may now be classified as some concrete type; therefore we
-- compute a classifier for a potentially /different/ type along with
-- evidence that we can coerce between the two.
data Reclassified o a where
  Reclassified :: o b -> FromUsr a b -> Reclassified o a

-- | Extension of 'Reclassified' to multiple elems
--
-- This is used internally only.
data ReclassifiedElems o as where
  RElems ::
       (SListI bs, Length bs ~ Length as)
    => Elems o bs -> PairWise FromUsr as bs -> ReclassifiedElems o as

reclassify_ :: forall m o o'. Applicative m
  => (forall a. o a -> m (Reclassified o' a))
  -> (forall a. Classifier_ o a -> m (Classifier_ (Reclassified o') a))
reclassify_ = mapClassifier

-- | Lift 'Reclassified' to the top-level
--
-- Given a classifier with user-defined classifiers at the levels, along with
-- coercion functions, leave the user-defined classifiers in place but lift the
-- coercion function to the top-level.
distribReclassified :: forall o.
     (forall a. Classifier_ (Reclassified o) a -> Reclassified (Classifier_ o) a)
distribReclassified = go
  where
    go :: forall a. Classifier_ (Reclassified o) a -> Reclassified (Classifier_ o) a
    -- Primitive and user-defined
    go (C_Prim  c) = Reclassified (C_Prim c) Id
    go (C_Other c) = case c of Reclassified c' f -> Reclassified (C_Other c') f

    -- Compound
    go (C_Maybe        c) = go1 C_Maybe        c
    go (C_Either       c) = go2 C_Either       c
    go (C_List         c) = go1 C_List         c
    go (C_Ratio        c) = go1 C_Ratio        c
    go (C_Set          c) = go1 C_Set          c
    go (C_Map          c) = go2 C_Map          c
    go (C_IntMap       c) = go1 C_IntMap       c
    go (C_Sequence     c) = go1 C_Sequence     c
    go (C_Tree         c) = go1 C_Tree         c
    go (C_HashSet      c) = go1 C_HashSet      c
    go (C_HashMap      c) = go2 C_HashMap      c
    go (C_HM_Array     c) = go1 C_HM_Array     c
    go (C_Prim_Array   c) = go1 C_Prim_Array   c
    go (C_Vector_Boxed c) = go1 C_Vector_Boxed c
    go (C_Tuple        c) = goN C_Tuple        c

    go1 :: forall f a.
         (forall a'. Elems o '[a'] -> Classifier_ o (f a'))
      -> Elems (Reclassified o) '[a]
      -> Reclassified (Classifier_ o) (f a)
    go1 cf c =
        case distribElems c of
          RElems c' (PCons f PNil) -> Reclassified (cf c') (F1 f)

    go2 :: forall f a b.
         (forall a' b'. Elems o '[a', b'] -> Classifier_ o (f a' b'))
      -> Elems (Reclassified o) '[a, b]
      -> Reclassified (Classifier_ o) (f a b)
    go2 cf c =
        case distribElems c of
          RElems c' (PCons f (PCons f' PNil)) -> Reclassified (cf c') (F2 f f')

    goN :: forall f as.
         SListI as
      => (forall as'.
               (SListI as', Length as' ~ Length as)
            => Elems o as' -> Classifier_ o (f as'))
      -> Elems (Reclassified o) as
      -> Reclassified (Classifier_ o) (f as)
    goN cf c =
        case distribElems c of
          RElems c' fs -> Reclassified (cf c') (FN fs)

distribElem :: Elem (Reclassified o) a -> Reclassified (Elem o) a
distribElem = \case
    NoElem -> Reclassified NoElem Absurd
    Elem c -> case distribReclassified c of
                Reclassified c' f -> Reclassified (Elem c') f

distribElems ::
     SListI xs
  => Elems (Reclassified o) xs -> ReclassifiedElems o xs
distribElems = \(Elems cs) -> go $ hmap distribElem cs
  where
    go :: NP (Reclassified (Elem o)) xs -> ReclassifiedElems o xs
    go Nil                      = RElems (Elems Nil) PNil
    go (Reclassified c f :* cs) = case go cs of
                                    RElems (Elems cs') fs' ->
                                      RElems (Elems (c :* cs')) (PCons f fs')

{-------------------------------------------------------------------------------
  Evidence that we are only doing conversions from Any
-------------------------------------------------------------------------------}

-- | Evidence that we can convert between two types
--
-- The only actual conversion we ever do is from 'UserDefined' (aka 'Any') to
-- whatever type the reclassification gives.
data FromUsr :: Type -> Type -> Type where
  Id      :: FromUsr a a
  Absurd  :: FromUsr Void a
  FromUsr :: FromUsr UserDefined a
  F1      :: FromUsr a1 b1 -> FromUsr (f a1) (f b1)
  F2      :: FromUsr a1 b1 -> FromUsr a2 b2 -> FromUsr (f a1 a2) (f b1 b2)
  FN      :: PairWise FromUsr as bs -> FromUsr (f as) (f bs)
  Compose :: FromUsr b c -> FromUsr a b -> FromUsr a c

-- | Coerce, given some evidence that the coercion is sound.
coerceFromUsr :: FromUsr a b -> a -> b
coerceFromUsr = unsafeCoerce
