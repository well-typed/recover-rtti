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
    => Classifiers_ o bs -> PairWise FromUsr as bs -> ReclassifiedElems o as

reclassify_ :: forall m o o'. Applicative m
  => (forall a. o a -> m (Reclassified o' a))
  -> (forall a. Classifier_ o a -> m (Classifier_ (Reclassified o') a))
reclassify_ = mapClassifier

-- | Lift 'Reclassified' to the top-level
--
-- Given a classifier with user-defined classifiers at the levels, along with
-- coercion functions, leave the user-defined classifiers in place but lift the
-- coercion function to the top-level.
distribReclassified :: forall o a.
     Classifier_ (Reclassified o) a
  -> Reclassified (Classifier_ o) a
distribReclassified = go
  where
    go :: forall x. Classifier_ (Reclassified o) x -> Reclassified (Classifier_ o) x

    -- Primitive and user-defined types

    go (C_Prim  c) = Reclassified (C_Prim c) Id
    go (C_Other c) = case c of Reclassified c' f -> Reclassified (C_Other c') f

    -- Compound types with unclassified elements
    go C_HashSet          = Reclassified C_HashSet          Id
    go C_IntMap           = Reclassified C_IntMap           Id
    go C_Maybe            = Reclassified C_Maybe            Id
    go C_Ratio            = Reclassified C_Ratio            Id
    go C_Set              = Reclassified C_Set              Id
    go C_Tree             = Reclassified C_Tree             Id

    go (C_HM_Array     c) = Reclassified (C_HM_Array     c) Id
    go (C_List         c) = Reclassified (C_List         c) Id
    go (C_Prim_Array   c) = Reclassified (C_Prim_Array   c) Id
    go (C_Sequence     c) = Reclassified (C_Sequence     c) Id
    go (C_Vector_Boxed c) = Reclassified (C_Vector_Boxed c) Id

    go C_Either           = Reclassified C_Either           Id
    go C_HashMap          = Reclassified C_HashMap          Id
    go C_Map              = Reclassified C_Map              Id

    -- Compound types with classified elements
    go (C_Tuple cs) = goN C_Tuple cs

    goN :: forall f xs.
         SListI xs
      => (forall xs'.
               (SListI xs', Length xs' ~ Length xs)
            => Classifiers_ o xs' -> Classifier_ o (f xs'))
      -> Classifiers_ (Reclassified o) xs
      -> Reclassified (Classifier_ o) (f xs)
    goN cf c =
        case distribElems c of
          RElems c' fs -> Reclassified (cf c') (FN fs)

distribElems ::
     SListI xs
  => Classifiers_ (Reclassified o) xs -> ReclassifiedElems o xs
distribElems = \(Classifiers_ cs) -> go $ hmap distribReclassified cs
  where
    go :: NP (Reclassified (Classifier_ o)) xs -> ReclassifiedElems o xs
    go Nil                      = RElems (Classifiers_ Nil) PNil
    go (Reclassified c f :* cs) =
        case go cs of
          RElems (Classifiers_ cs') fs' ->
            RElems (Classifiers_ (c :* cs')) (PCons f fs')


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
