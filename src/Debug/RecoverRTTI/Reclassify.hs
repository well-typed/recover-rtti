{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Support for reclassification
module Debug.RecoverRTTI.Reclassify (
    Reclassified(..)
  , reclassify_
  , distribReclassified
  ) where

import Data.Bifunctor
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.SOP hiding (NS(..))
import Data.Void
import GHC.Real
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Nat

-- | Reclassified values
--
-- Reclassification can be done by user code which want to take advantage of
-- the classification infrastructure for @recover-rtti@ but add some additional
-- classification for domain-specific types known only to that client code.
--
-- When we reclassify a value, a value that might previously be classified as
-- @UserDefined@ may now be classified as some concrete type; therefore we
-- compute a classifier for a potentially /different/ type along with a
-- coercion between the two.
data Reclassified o a where
  Reclassified :: o b -> (a -> b) -> Reclassified o a

-- | Extension of 'Reclassified' to multiple elems
--
-- This is used internally only.
data ReclassifiedElems o as where
  RElems ::
       (SListI bs, Length bs ~ Length as)
    => Elems o bs -> PairWise as bs -> ReclassifiedElems o as

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
    go (C_Prim  c) = Reclassified (C_Prim c) id
    go (C_Other c) = case c of Reclassified c' f -> Reclassified (C_Other c') f

    -- Compound
    go (C_Maybe        c) = go1 C_Maybe        fmap          c
    go (C_Either       c) = go2 C_Either       bimap         c
    go (C_List         c) = go1 C_List         fmap          c
    go (C_Ratio        c) = go1 C_Ratio        coerceRatio   c
    go (C_Set          c) = go1 C_Set          coerceSet     c
    go (C_Map          c) = go2 C_Map          coerceMap     c
    go (C_IntMap       c) = go1 C_IntMap       fmap          c
    go (C_Sequence     c) = go1 C_Sequence     fmap          c
    go (C_Tree         c) = go1 C_Tree         fmap          c
    go (C_HashSet      c) = go1 C_HashSet      coerceHashSet c
    go (C_HashMap      c) = go2 C_HashMap      coerceHashMap c
    go (C_HM_Array     c) = go1 C_HM_Array     coerceHMArray c
    go (C_Prim_Array   c) = go1 C_Prim_Array   fmap          c
    go (C_Vector_Boxed c) = go1 C_Vector_Boxed fmap          c
    go (C_Tuple        c) = goN C_Tuple        mapTuple      c

    go1 :: forall f a.
         (forall a'. Elems o '[a'] -> Classifier_ o (f a'))
      -> (forall a'. (a -> a') -> f a -> f a')
      -> Elems (Reclassified o) '[a]
      -> Reclassified (Classifier_ o) (f a)
    go1 cf mapf c =
        case distribElems c of
          RElems c' (PCons f PNil) -> Reclassified (cf c') (mapf f)

    go2 :: forall f a b.
         (forall a' b'. Elems o '[a', b'] -> Classifier_ o (f a' b'))
      -> (forall a' b'. (a -> a') -> (b -> b') -> f a b -> f a' b')
      -> Elems (Reclassified o) '[a, b]
      -> Reclassified (Classifier_ o) (f a b)
    go2 cf mapf c =
        case distribElems c of
          RElems c' (PCons f (PCons f' PNil)) -> Reclassified (cf c') (mapf f f')

    goN :: forall f as.
         SListI as
      => (forall as'.
               (SListI as', Length as' ~ Length as)
            => Elems o as' -> Classifier_ o (f as'))
      -> (forall as'. PairWise as as' -> f as -> f as')
      -> Elems (Reclassified o) as
      -> Reclassified (Classifier_ o) (f as)
    goN cf mapf c =
        case distribElems c of
          RElems c' fs -> Reclassified (cf c') (mapf fs)

distribElem :: Elem (Reclassified o) a -> Reclassified (Elem o) a
distribElem = \case
    NoElem -> Reclassified NoElem absurd
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
  Lift coercions to non-functor types
-------------------------------------------------------------------------------}

coerceRatio :: (x -> x') -> Ratio x -> Ratio x'
coerceRatio f (x :% y) = f x :% f y

coerceSet :: (x -> x') -> Set x -> Set x'
coerceSet f = Set.fromDistinctAscList . map f . Set.toAscList

coerceMap :: (x -> x') -> (y -> y') -> Map x y -> Map x' y'
coerceMap f g = Map.fromDistinctAscList . map (bimap f g) . Map.toAscList

coerceHMArray :: (x -> x') -> HashMap.Array x -> HashMap.Array x'
coerceHMArray f arr =
    let xs = HashMap.Array.toList arr
    in HashMap.Array.fromList (length xs) (map f xs)

-- Unfortunately, coercion on HashSet/HashMap is not expressible using its API
coerceHashSet :: (x -> x') -> HashSet x -> HashSet x'
coerceHashSet _ = unsafeCoerce

coerceHashMap :: (x -> x') -> (y -> y') -> HashMap x y -> HashMap x' y'
coerceHashMap _ _ = unsafeCoerce
