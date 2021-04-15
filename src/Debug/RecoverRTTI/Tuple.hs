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
  , unwrapTuple
    -- * Conversion between tuples and NP
  , tupleFromNP
  , tupleToNP
    -- * Mapping
  , PairWise(..)
  , mapTuple
    -- * Re-exports
  , module Debug.RecoverRTTI.Tuple.Recursive
  , module Debug.RecoverRTTI.Tuple.Size
  ) where

import Data.SOP hiding (NS(..))

import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Tuple.Recursive
import Debug.RecoverRTTI.Tuple.Size

{-------------------------------------------------------------------------------
  Wrapped tuple

  NOTE: We cannot add any dictionaries in @WrappedTuple@ itself, it /MUST/ be
  a type synonym: it is critical that we can 'unsafeCoerce' a regular tuple to a
  wrapped tuple.
-------------------------------------------------------------------------------}

-- | Inductive tuple
--
-- Inductive view on tuples that can be constructed with or pattern matched on
-- using 'TNil' and 'TCons'. The underlying representation is a /true/ tuple
-- however; for example, @Tuple '[Int, Bool, Char] ~ (Int, Bool, Char)@.
newtype WrappedTuple xs = WrappedTuple { unwrapTuple :: Tuple xs }

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
  Mapping
-------------------------------------------------------------------------------}

data PairWise xs ys where
  PNil  :: PairWise '[] '[]
  PCons :: (x -> y) -> PairWise xs ys -> PairWise (x:xs) (y:ys)

data SameListShape xs ys where
  SameListShape :: (SListI ys, Length xs ~ Length ys) => SameListShape xs ys

mapTuple' ::
     (SListI xs, IsValidSize (Length xs))
  => PairWise xs ys -> WrappedTuple xs -> (SameListShape xs ys, WrappedTuple ys)
mapTuple' PNil          TNil        = (SameListShape, TNil)
mapTuple' (PCons f fs) (TCons x xs) =
    case mapTuple' fs xs of
      (SameListShape, ys) -> (SameListShape, TCons (f x) ys)

mapTuple ::
     (SListI xs, IsValidSize (Length xs))
  => PairWise xs ys -> WrappedTuple xs -> WrappedTuple ys
mapTuple fs = snd . mapTuple' fs

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
