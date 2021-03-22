{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.RecoverRTTI.QuickCheck.DepGen (
    -- * Dependent generator
    DepGen(..)
  , depClassifier
  , depGen
    -- * Creation
  , arbitraryDepGen
  , primDepGen
    -- * Bundle a dependent generator with a lifting function
  , GenJust(..)
  , GenLeft(..)
  , GenRight(..)
  , GenPair(..)
  , GenNP(..)
  , genJust
  , genLeft
  , genRight
  , genPair
  , genNP
  ) where

import Data.Kind
import Data.SOP
import Data.SOP.Dict
import Data.Void

import Debug.RecoverRTTI

import Test.QuickCheck

import Test.RecoverRTTI.Prim
import Test.RecoverRTTI.QuickCheck.Sized (SizedGen)

import qualified Test.RecoverRTTI.QuickCheck.Sized as SG

{-------------------------------------------------------------------------------
  Dependent generator
-------------------------------------------------------------------------------}

-- | Dependent generator
data DepGen c a where
  DepGen :: (Show a, Eq a) => c a -> SizedGen a -> DepGen c a

depClassifier :: DepGen c a -> c a
depClassifier (DepGen c _) = c

depGen :: DepGen c a -> SizedGen a
depGen (DepGen _ gen) = gen

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

arbitraryDepGen :: (Arbitrary a, Show a, Eq a) => c a -> DepGen c a
arbitraryDepGen cc = DepGen cc $ SG.arbitrary

primDepGen :: PrimClassifier a -> DepGen (Classifier_ o) a
primDepGen C_String = DepGen (C_Prim C_String) $ SG.lift $
    arbitrary `suchThat` (not . null) -- empty string classified as @[Void]@
primDepGen c =
    case (primSatisfiesArbitrary c, canShowPrim c, canComparePrim c) of
      (Dict, Dict, Dict) -> arbitraryDepGen (C_Prim c)

{-------------------------------------------------------------------------------
  Bundle a dependent generator with a lifting function

  These are designed to work with 'MaybeF' and co.
-------------------------------------------------------------------------------}

data GenJust c (f :: Type -> Type) a = GenJust {
      justGen  :: SizedGen a -> SizedGen (f a)
    , justElem :: DepGen c a
    }

data GenLeft c (f :: Type -> Type -> Type) a = GenLeft {
      leftGen  :: SizedGen a -> SizedGen (f a Void)
    , leftElem :: DepGen c a
    }

data GenRight c (f :: Type -> Type -> Type) b = GenRight {
      rightGen  :: SizedGen b -> SizedGen (f Void b)
    , rightElem :: DepGen c b
    }

data GenPair c (f :: Type -> Type -> Type) (ab :: (Type, Type)) where
    GenPair :: forall c f a b. {
           pairGen :: SizedGen a -> SizedGen b -> SizedGen (f a b)
         , pairFst :: DepGen c a
         , pairSnd :: DepGen c b
         }
      -> GenPair c f '(a, b)

data GenNP c f xs = GenNP {
      npGen  :: NP SizedGen xs -> SizedGen (f xs)
    , npElem :: NP (DepGen c) xs
    }

genJust ::
     ( forall x. Show x => Show (f x)
     , forall x. Eq   x => Eq   (f x)
     )
  => (c a -> c' (f a)) -> GenJust c f a -> DepGen c' (f a)
genJust cf (GenJust gen (DepGen cx gx)) =
    DepGen (cf cx) (gen gx)

genLeft ::
     ( forall x y. (Show x, Show y) => Show (f x y)
     , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
     )
  => (c a -> c' (f a Void)) -> GenLeft c f a -> DepGen c' (f a Void)
genLeft cf (GenLeft gen (DepGen cx gx)) =
    DepGen (cf cx) (gen gx)

genRight ::
     ( forall x y. (Show x, Show y) => Show (f x y)
     , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
     )
  => (c b -> c' (f Void b)) -> GenRight c f b -> DepGen c' (f Void b)
genRight cf (GenRight gen (DepGen cy gy)) =
    DepGen (cf cy) (gen gy)

genPair ::
     ( forall x y. (Show x, Show y) => Show (f x y)
     , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
     )
  => ((c a, c b) -> c' (f a b)) -> GenPair c f '(a, b) -> DepGen c' (f a b)
genPair cf (GenPair gen (DepGen cx gx) (DepGen cy gy)) =
    DepGen (cf (cx, cy)) $
      gen (SG.withSize (`div` 2) gx)
          (SG.withSize (`div` 2) gy)

genNP :: forall c c' f xs.
     ( SListI xs
     , All Show xs => Show (f xs )
     , All Eq   xs => Eq   (f xs)
     )
  => (NP c xs -> c' (f xs)) -> GenNP c f xs -> DepGen c' (f xs)
genNP cf (GenNP gen elems) =
    case (all_NP allShow, all_NP allEq) of
      (Dict, Dict) ->
        DepGen
          (cf (hmap depClassifier elems))
          (gen (hmap (SG.withSize divSize . depGen) elems))
  where
    divSize :: Int -> Int
    divSize sz = (sz - 1) `div` lengthSList (Proxy @xs)

    allShow :: NP (Dict Show) xs
    allShow = hmap (\DepGen{} -> Dict) elems

    allEq :: NP (Dict Eq) xs
    allEq = hmap (\DepGen{} -> Dict) elems
