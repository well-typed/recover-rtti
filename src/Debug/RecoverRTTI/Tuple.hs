{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Debug.RecoverRTTI.Tuple (
    -- * Wrapped tuple
    WrappedTuple(WrappedTuple, TNil, TCons)
    -- * Auxiliary
  , bimapTuple
    -- * Conversion between tuples and NP
  , tupleFromNP
  , tupleToNP
    -- * Re-exports
  , module Debug.RecoverRTTI.Tuple.Recursive
  , module Debug.RecoverRTTI.Tuple.Size
  ) where

import Data.SOP hiding (NS(..))

import Debug.RecoverRTTI.Tuple.Recursive
import Debug.RecoverRTTI.Tuple.Size
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Wrapped tuple

  NOTE: We cannot add any dictionaries in @WrappedTuple@ itself, it /MUST/ be
  a type synonym: it is critical that we can 'unsafeCoerce' a regular tuple to a
  wrapped tuple.
-------------------------------------------------------------------------------}

newtype WrappedTuple xs = WrappedTuple (Tuple xs)

pattern TNil ::
     forall xs. (SListI xs, IsValidSize (Length xs))
  => xs ~ '[]
  => WrappedTuple xs
pattern TNil <- (viewWrapped -> TupleEmpty)
  where
    TNil = WrappedTuple ()

pattern TCons ::
     forall   xs'. (SListI xs', IsValidSize (Length xs'))
  => forall x xs . (xs' ~ (x ': xs), SListI xs, IsValidSize (Length xs))
  => x -> WrappedTuple xs -> WrappedTuple xs'
pattern TCons x xs <- (viewWrapped -> TupleNonEmpty x xs)
  where
    TCons x xs = consWrapped (x, xs)

{-# COMPLETE TNil, TCons #-}

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

{-------------------------------------------------------------------------------
  Conversion to/from NP
-------------------------------------------------------------------------------}

tupleFromNP :: forall xs.
     (SListI xs, IsValidSize (Length xs))
  => NP I xs -> WrappedTuple xs
tupleFromNP Nil         = TNil
tupleFromNP (I x :* xs) = smallerIsValid (Proxy @(Length xs))
                        $ TCons x (tupleFromNP xs)

tupleToNP ::
     (SListI xs, IsValidSize (Length xs))
  => WrappedTuple xs -> NP I xs
tupleToNP TNil         = Nil
tupleToNP (TCons x xs) = I x :* tupleToNP xs

{-------------------------------------------------------------------------------
  Internal auxiliary functions for defining the pattern synonym
-------------------------------------------------------------------------------}

data TupleView xs where
  TupleEmpty    :: TupleView '[]
  TupleNonEmpty :: (SListI xs, IsValidSize (Length xs))
                => x -> WrappedTuple xs -> TupleView (x ': xs)

viewWrapped ::
     (SListI xs, IsValidSize (Length xs))
  => WrappedTuple xs
  -> TupleView xs
viewWrapped (WrappedTuple t) =
    go sList t
  where
    go :: forall xs.
         IsValidSize (Length xs)
      => SList xs -> Tuple xs -> TupleView xs
    go SNil  () = TupleEmpty
    go SCons xs = goCons xs

    goCons :: forall x xs.
         (SListI xs, IsValidSize (Length (x ': xs)))
      => Tuple (x ': xs) -> TupleView (x ': xs)
    goCons xs =
        smallerIsValid (Proxy @(Length (x ': xs))) $
          TupleNonEmpty x (WrappedTuple xs')
      where
        (x, xs') = uncons (Proxy @xs) isValidSize xs

consWrapped :: forall x xs.
     (SListI xs, IsValidSize (Length (x ': xs)))
  => (x, WrappedTuple xs) -> WrappedTuple (x ': xs)
consWrapped (x, WrappedTuple xs) =
    WrappedTuple (cons (Proxy @xs) isValidSize (x, xs))

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance ( SListI xs
         , IsValidSize (Length xs)
         , All Show xs
         ) => Show (WrappedTuple xs) where
  showsPrec _ =
        show_tuple
      . hcollapse
      . hcmap (Proxy @Show) (mapIK shows)
      . tupleToNP
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
  (tupleToNP -> xs) == (tupleToNP -> ys) =
       and . hcollapse $ hczipWith (Proxy @Eq) (mapIIK (==)) xs ys
