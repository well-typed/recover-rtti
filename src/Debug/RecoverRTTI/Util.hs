{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Debug.RecoverRTTI.Util (
    -- * Existentials
    Some(..)
  , elimKnownSymbol
    -- * Constraints
  , keepRedundantConstraint
    -- * Traversable
  , checkEmptyTraversable
    -- * Lists
  , dropEnds
    -- * SOP
  , VerifiedSize(..)
  , verifySize
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP
import Data.Void
import GHC.TypeLits (KnownSymbol, SomeSymbol(..), someSymbolVal)

import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Existentials
-------------------------------------------------------------------------------}

data Some (f :: k -> Type) where
  Some :: forall f a. f a -> Some f

elimKnownSymbol :: String -> (forall n. KnownSymbol n => Proxy n -> r) -> r
elimKnownSymbol s k =
    case someSymbolVal s of
      SomeSymbol p -> k p

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

-- | Can be used to silence individual "redundant constraint" warnings
--
-- > foo :: ConstraintUsefulForDebugging => ...
-- > foo =
-- >     ..
-- >   where
-- >     _ = keepRedundantConstraint (Proxy @ConstraintUsefulForDebugging))
keepRedundantConstraint :: c => proxy c -> ()
keepRedundantConstraint _ = ()

{-------------------------------------------------------------------------------
  Traversable
-------------------------------------------------------------------------------}

-- | Check if a traversable data structure is empty
--
-- Returns evidence: an element of the data-structure if it's non-empty,
-- or evidence that it is empty otherwise.
checkEmptyTraversable :: Traversable t => t a -> Either a (t Void)
checkEmptyTraversable = traverse Left

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

verifySize :: Sing n -> [a] -> Maybe (VerifiedSize n a)
verifySize = go
  where
    go :: Sing n -> [a] -> Maybe (VerifiedSize n a)
    go SZ     []     = Just (VerifiedSize Nil)
    go (SS n) (x:xs) = do VerifiedSize np <- go n xs
                          return $ VerifiedSize (K x :* np)
    go SZ     (_:_)  = Nothing
    go (SS _) []     = Nothing
