{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug.RecoverRTTI.Tuple (
    -- * Conversion between tuples and NP
    tupleFromNP
  , tupleToNP
    -- * Wrapped tuple
  , WrappedTuple(..)
    -- * Re-exports
  , module Debug.RecoverRTTI.Tuple.Recursive
  , module Debug.RecoverRTTI.Tuple.Size
  ) where

import Data.SOP hiding (NS(..))

import Debug.RecoverRTTI.Tuple.Recursive
import Debug.RecoverRTTI.Tuple.Size
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

tupleFromNP ::
     (SListI xs, IsValidSize (Length xs))
  => NP I xs -> Tuple xs
tupleFromNP = go isValidSize
  where
    go :: SListI xs => ValidSize (Length xs) -> NP I xs -> Tuple xs
    go _     Nil         = ()
    go valid (I x :* xs) = goCons valid x xs

    goCons :: forall x xs.
         SListI xs
      => ValidSize ('S (Length xs))
      -> x -> NP I xs -> Tuple (x : xs)
    goCons valid x xs = cons (Proxy @xs) valid (x, go (smallerIsValid valid) xs)

tupleToNP ::
     (SListI xs, IsValidSize (Length xs))
  => Proxy xs -> Tuple xs -> NP I xs
tupleToNP _ = go sList isValidSize
  where
    go :: SList xs -> ValidSize (Length xs) -> Tuple xs -> NP I xs
    go SNil  _     () = Nil
    go SCons valid xs = goCons valid xs

    goCons :: forall x xs.
         SListI xs
      => ValidSize ('S (Length xs))
      -> Tuple (x : xs) -> NP I (x : xs)
    goCons valid xs =
        let (x, xs') = uncons (Proxy @xs) valid xs
        in I x :* go sList (smallerIsValid valid) xs'

{-------------------------------------------------------------------------------
  Wrapped tuple
-------------------------------------------------------------------------------}

newtype WrappedTuple xs = WrappedTuple {
      getWrappedTuple :: Tuple xs
    }

instance ( SListI xs
         , IsValidSize (Length xs)
         , All Show xs
         ) => Show (WrappedTuple xs) where
  showsPrec _ =
        show_tuple
      . hcollapse
      . hcmap (Proxy @Show) (mapIK shows)
      . tupleToNP (Proxy @xs)
      . getWrappedTuple
    where
      -- Copied from @GHC.Show@ (not exported)
      show_tuple :: [ShowS] -> ShowS
      show_tuple ss = showChar '('
                    . foldr1 (\s r -> s . showChar ',' . r) ss
                    . showChar ')'

instance ( SListI xs
         , IsValidSize (Length xs)
         , All Eq xs
         ) => Eq (WrappedTuple xs) where
  (==) = \(WrappedTuple xs) (WrappedTuple ys) ->
      go (tupleToNP (Proxy @xs) xs) (tupleToNP (Proxy @xs) ys)
    where
      go :: NP I xs -> NP I xs -> Bool
      go xs ys = and . hcollapse $ hczipWith (Proxy @Eq) (mapIIK (==)) xs ys
