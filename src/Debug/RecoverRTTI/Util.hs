{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Debug.RecoverRTTI.Util (
    -- * Existentials
    Some(..)
  , elimKnownSymbol
  ) where

import Data.Kind
import Data.Proxy
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
