{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Debug.RecoverRTTI.Util (
    -- * Existentials
    Some(..)
  , mapSome
    -- * Lists
  , dropEnds
    -- * SOP
  , VerifiedSize(..)
  , verifySize
  ) where

import Data.Kind
import Data.SOP

import Debug.RecoverRTTI.Nat

{-------------------------------------------------------------------------------
  Existentials
-------------------------------------------------------------------------------}

data Some (f :: k -> Type) where
  Some :: forall f a. f a -> Some f

mapSome :: (forall x. f x -> g x) -> Some f -> Some g
mapSome f (Some x) = Some (f x)

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Drop the ends of a list
--
-- > dropEnds "abcde" == Just ('a',"bcd",'e')
dropEnds :: forall a. [a] -> Maybe (a, [a], a)
dropEnds = \case
    []     -> Nothing
    (a:xs) -> go a xs
  where
    go :: a -> [a] -> Maybe (a, [a], a)
    go a = goRest []
      where
        goRest :: [a] -> [a] -> Maybe (a, [a], a)
        goRest _   []     = Nothing
        goRest acc [z]    = Just (a, reverse acc, z)
        goRest acc (x:xs) = goRest (x:acc) xs

{-------------------------------------------------------------------------------
  SOP
-------------------------------------------------------------------------------}

data VerifiedSize (n :: Nat) (a :: Type) where
    -- This is intentionally not kind polymorphic
    VerifiedSize :: forall n a (xs :: [Type]).
         (SListI xs, Length xs ~ n)
      => NP (K a) xs -> VerifiedSize n a

verifySize :: SNat n -> [a] -> Maybe (VerifiedSize n a)
verifySize = go
  where
    go :: SNat n -> [a] -> Maybe (VerifiedSize n a)
    go SZ     []     = Just (VerifiedSize Nil)
    go (SS n) (x:xs) = do VerifiedSize np <- go n xs
                          return $ VerifiedSize (K x :* np)
    go SZ     (_:_)  = Nothing
    go (SS _) []     = Nothing
