{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Support for reclassification
module Debug.RecoverRTTI.Reclassify (
    Reclassified(..)
  , reclassify_
  , distribReclassified
  ) where

import Data.Bifunctor
import Data.Void
import GHC.Real
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.SOP hiding (NS(..))
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

reclassify_ :: forall m o o'. Applicative m
  => (forall a. o a -> m (Reclassified o' a))
  -> (forall a. Classifier_ o a -> m (Classifier_ (Reclassified o') a))
reclassify_ = mapClassifier

distribReclassified :: forall o.
     (forall a. Classifier_ (Reclassified o) a -> Reclassified (Classifier_ o) a)
distribReclassified = go
  where
    go :: forall a. Classifier_ (Reclassified o) a -> Reclassified (Classifier_ o) a
    -- Primitive and user-defined
    go (C_Prim  c) = Reclassified (C_Prim c) id
    go (C_Other c) = case c of Reclassified c' f -> Reclassified (C_Other c') f

    -- Compound
    go (C_Maybe           c) = goMaybeF     C_Maybe           fmap          c
    go (C_Either          c) = goEitherF    C_Either          bimap         c
    go (C_List            c) = goMaybeF     C_List            fmap          c
    go (C_Ratio           c) = goF          C_Ratio           coerceRatio   c
    go (C_Set             c) = goMaybeF     C_Set             coerceSet     c
    go (C_Map             c) = goMaybePairF C_Map             coerceMap     c
    go (C_IntMap          c) = goMaybeF     C_IntMap          fmap          c
    go (C_Sequence        c) = goMaybeF     C_Sequence        fmap          c
    go (C_Tree            c) = goF          C_Tree            fmap          c
    go (C_HashSet         c) = goF          C_HashSet         coerceHashSet c
    go (C_HashMap         c) = goMaybePairF C_HashMap         coerceHashMap c
    go (C_HM_Array        c) = goMaybeF     C_HM_Array        coerceHMArray c
    go (C_Prim_Array      c) = goMaybeF     C_Prim_Array      fmap          c
    go (C_Vector_Boxed    c) = goMaybeF     C_Vector_Boxed    fmap          c
    go (C_Vector_Unboxed  c) = Reclassified (C_Vector_Unboxed  c) id
    go (C_Vector_UnboxedM c) = Reclassified (C_Vector_UnboxedM c) id
    go (C_Tuple           c) = goTuple                                      c

    goF :: forall f.
         (forall a. Classifier_ o a -> Classifier_ o (f a))
      -> (forall a b. (a -> b) -> f a -> f b)
      -> (forall a. Classifier_ (Reclassified o) a -> Reclassified (Classifier_ o) (f a))
    goF cc coerce c = lift $ go c
      where
        lift ::
             Reclassified (Classifier_ o) a
          -> Reclassified (Classifier_ o) (f a)
        lift (Reclassified c' f) = Reclassified (cc c') (coerce f)

    goMaybeF :: forall f.
         (forall a. MaybeF o a -> Classifier_ o (f a))
      -> (forall a b. (a -> b) -> f a -> f b)
      -> (forall a. MaybeF (Reclassified o) a -> Reclassified (Classifier_ o) (f a))
    goMaybeF cc _      FNothing  = Reclassified (cc FNothing) id
    goMaybeF cc coerce (FJust c) = lift $ go c
      where
        lift ::
             Reclassified (Classifier_ o) a
          -> Reclassified (Classifier_ o) (f a)
        lift (Reclassified c' f) = Reclassified (cc (FJust c')) (coerce f)

    goEitherF :: forall f.
         (forall a b. EitherF o a b -> Classifier_ o (f a b))
      -> (forall a a' b b'. (a -> b) -> (a' -> b') -> f a a' -> f b b')
      -> (forall a b. EitherF (Reclassified o) a b -> Reclassified (Classifier_ o) (f a b))
    goEitherF cc coerce (FLeft c) = lift $ go c
      where
        lift ::
             Reclassified (Classifier_ o) a
          -> Reclassified (Classifier_ o) (f a Void)
        lift (Reclassified c' f) = Reclassified (cc (FLeft c')) (coerce f id)
    goEitherF cc coerce (FRight c) = lift $ go c
      where
        lift ::
             Reclassified (Classifier_ o) b
          -> Reclassified (Classifier_ o) (f Void b)
        lift (Reclassified c' f) = Reclassified (cc (FRight c')) (coerce id f)

    goMaybePairF :: forall f.
         (forall a b. MaybePairF o a b -> Classifier_ o (f a b))
      -> (forall a a' b b'. (a -> b) -> (a' -> b') -> f a a' -> f b b')
      -> (forall a b. MaybePairF (Reclassified o) a b -> Reclassified (Classifier_ o) (f a b))
    goMaybePairF cc _      FNothingPair      = Reclassified (cc FNothingPair) id
    goMaybePairF cc coerce (FJustPair c1 c2) = lift (go c1) (go c2)
      where
        lift ::
             Reclassified (Classifier_ o) a
          -> Reclassified (Classifier_ o) b
          -> Reclassified (Classifier_ o) (f a b)
        lift (Reclassified c1' f) (Reclassified c2' g) =
            Reclassified (cc (FJustPair c1' c2')) (coerce f g)

    goTuple ::
         (SListI xs, IsValidSize (Length xs))
      => Classifiers (Reclassified o) xs
      -> Reclassified (Classifier_ o) (WrappedTuple xs)
    goTuple (Classifiers cs) =
        lift cs $ \cs' f -> Reclassified (C_Tuple (Classifiers cs')) f
      where
        lift :: forall xs r.
             (SListI xs, IsValidSize (Length xs))
          => NP (Classifier_ (Reclassified o)) xs
          -> (forall ys.
                   (SListI ys, Length ys ~ Length xs)
                => NP (Classifier_ o) ys
                -> (WrappedTuple xs -> WrappedTuple ys)
                -> r
             )
          -> r
        lift Nil       k = k Nil id
        lift (x :* xs) k = smallerIsValid (Proxy @(Length xs)) $
                             lift xs $ \np f_np ->
                               case go x of
                                 Reclassified y f_y ->
                                   k (y :* np) (bimapTuple f_y f_np)

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

bimapTuple ::
      ( SListI xs
      , SListI ys
      , IsValidSize (Length (x ': xs))
      , Length xs ~ Length ys
      )
   => (x -> y)
   -> (WrappedTuple xs -> WrappedTuple ys)
   -> WrappedTuple (x ': xs) -> WrappedTuple (y ': ys)
bimapTuple f g (TCons x xs) = TCons (f x) (g xs)
