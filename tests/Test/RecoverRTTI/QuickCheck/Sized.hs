{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- | Sized generators
--
-- Intended for qualified import
--
-- > import Test.RecoverRTTI.QuickCheck.Sized (SizedGen)
-- > import qualified Test.RecoverRTTI.QuickCheck.Sized as SG
module Test.RecoverRTTI.QuickCheck.Sized (
    -- * Sized generators
    SizedGen(..)
  , run
    -- * Lifting @Gen@ into @SizedGen@
  , lift
  , arbitrary
    -- * Combinators
  , suchThat
  , withSize
  , leafOrStep
  , oneofStepped
  , replicate
  , divvy
  , divvyPair
    -- ** Derived
  , genListLike
  , genMapLike
    -- ** Support for tuples
  , ValidTuple(..)
  , genTuple
  ) where

import Prelude hiding (replicate)

import Data.Kind
import Data.SOP
import Data.SOP.Dict

import Debug.RecoverRTTI

import Test.QuickCheck (Arbitrary, Gen)

import qualified Test.QuickCheck as QC

{-------------------------------------------------------------------------------
  Sized generators
-------------------------------------------------------------------------------}

-- | Sized generators
--
-- We thread the size all the way through the generating, to avoid generating
-- very big trees. This is nonetheless a naive approach; we might want to look
-- at papers such as "Feat: Functional Enumeration of Algebraic Types".
newtype SizedGen a = SizedGen { unSizedGen :: Int -> Gen a }
  deriving (Functor)

instance Applicative SizedGen where
  pure x  = SizedGen $ \_sz -> pure x
  f <*> x = SizedGen $ \ sz -> unSizedGen f sz <*> unSizedGen x sz

run :: Int -> SizedGen a -> Gen a
run n (SizedGen gen) = gen n

{-------------------------------------------------------------------------------
  Lifting @Gen@ into @SizedGen@
-------------------------------------------------------------------------------}

lift :: Gen a -> SizedGen a
lift gen = SizedGen $ \_ -> gen

arbitrary :: Arbitrary a => SizedGen a
arbitrary = lift QC.arbitrary

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

suchThat :: SizedGen a -> (a -> Bool) -> SizedGen a
gen `suchThat` p = SizedGen $ \sz -> unSizedGen gen sz `QC.suchThat` p

withSize :: (Int -> Int) -> SizedGen a -> SizedGen a
withSize f gen = SizedGen $ unSizedGen gen . f

leafOrStep :: Gen a -> [SizedGen a] -> SizedGen a
leafOrStep leaf nested = SizedGen $ \sz ->
    if sz > 1
      then QC.oneof (map (run (sz - 1)) nested)
      else leaf

oneofStepped :: [SizedGen a] -> SizedGen a
oneofStepped gens = SizedGen $ \sz -> QC.oneof $ map (run (sz - 1)) gens

replicate :: (Int, Int) -> SizedGen a -> SizedGen [a]
replicate (lo, hi) gen = SizedGen $ \sz -> do
    n <- QC.choose (lo, max lo (min sz hi))
    let sz' = (sz - 1) `div` n
    QC.vectorOf n $ run sz' gen

divvy :: forall xs. SListI xs => NP SizedGen xs -> SizedGen (NP I xs)
divvy = hsequence . hmap (withSize (`div` n))
  where
    n = lengthSList (Proxy @xs)

divvyPair :: SizedGen a -> SizedGen b -> SizedGen (a, b)
divvyPair ga gb = unwrapTuple . tupleFromNP <$> divvy (ga :* gb :* Nil)

{-------------------------------------------------------------------------------
  Derived combinators
-------------------------------------------------------------------------------}

genListLike :: ([a] -> x) -> SizedGen a -> SizedGen x
genListLike f = fmap f . replicate (1, 5)

genMapLike :: ([(a, b)] -> x) -> SizedGen a -> SizedGen b -> SizedGen x
genMapLike f genA genB = fmap f $ replicate (1, 5) $ divvyPair genA genB

{-------------------------------------------------------------------------------
  Support for tuples
-------------------------------------------------------------------------------}

data ValidTuple f (xs :: [Type]) where
  ValidTuple :: (SListI xs, IsValidSize (Length xs)) => NP f xs -> ValidTuple f x

-- | Generate arbitrary tuple
--
-- Precondition: the generator must be able to generate values for @size >= 1@.
genTuple :: forall f. SizedGen (Some f) -> SizedGen (Some (ValidTuple f))
genTuple gen = SizedGen $ \sz -> do
    -- Pick no less than 2, and no more than 62
    n <- QC.choose (2, max 2 (min 62 sz))
    case toValidSize n of
      Nothing -> error "impossible, we pick a valid tuple size"
      Just (Some validSize@(ValidSize n' _)) ->
        case liftValidSize validSize of
          Dict -> go (sz `div` n) n' $ return . Some . ValidTuple
  where
    go :: Int
       -> SNat n
       -> (forall xs. (SListI xs, Length xs ~ n) => NP f xs -> Gen r)
       -> Gen r
    go _    SZ     k = k Nil
    go sz' (SS s) k = go sz' s $ \xs -> do
        Some x <- run sz' gen
        k $ (x :* xs)
