{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Debug.RecoverRTTI.Util (
    -- * Existentials
    Some(..)
  , elimKnownSymbol
    -- * Constraints
  , keepRedundantConstraint
    -- * Traversable
  , checkEmptyTraversable
  ) where

import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeLits

{-------------------------------------------------------------------------------
  Existentials
-------------------------------------------------------------------------------}

data Some (f :: k -> Type) where
  Exists :: forall f (a :: k). f a -> Some f

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
