{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Establish that a constraint holds for all classified types
module Debug.RecoverRTTI.Constraint (
    PrimSatisfies
  , primSatisfies
  , ClassifiedSatisfies
  , classifiedSatisfies
  ) where

import Data.Aeson (Value)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Kind
import Data.Map (Map)
import Data.Ratio
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.SOP
import Data.SOP.Dict
import Data.Tree (Tree)
import Data.Void
import Data.Word

import qualified Data.ByteString             as BS.Strict
import qualified Data.ByteString.Lazy        as BS.Lazy
import qualified Data.ByteString.Short       as BS.Short
import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.Primitive.Array        as Prim (Array)
import qualified Data.Text                   as Text.Strict
import qualified Data.Text.Lazy              as Text.Lazy
import qualified Data.Vector                 as Vector.Boxed

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Wrappers

{-------------------------------------------------------------------------------
  Primitives
-------------------------------------------------------------------------------}

type PrimSatisfies (c :: Type -> Constraint) = (
  -- Primitive types

    c Bool
  , c Char
  , c Double
  , c Float
  , c Int
  , c Int16
  , c Int8
  , c Int32
  , c Int64
  , c Integer
  , c Ordering
  , c ()
  , c Word
  , c Word8
  , c Word16
  , c Word32
  , c Word64

  -- String types

  , c String
  , c BS.Strict.ByteString
  , c BS.Lazy.ByteString
  , c BS.Short.ShortByteString
  , c Text.Strict.Text
  , c Text.Lazy.Text

  -- Aeson

  , c Value

  -- Reference cells

  , c SomeSTRef
  , c SomeTVar
  , c SomeMVar

  -- Functions

  , c SomeFun

  -- Containers with no type arguments

  , c IntSet
  , c SomePrimArrayM
  , c SomeStorableVector
  , c SomeStorableVectorM
  , c SomePrimitiveVector
  , c SomePrimitiveVectorM
  )

primSatisfies :: forall c.
     PrimSatisfies c
  => (forall a. PrimClassifier a -> Dict c a)
primSatisfies = go
  where
    go :: PrimClassifier a -> Dict c a

    -- Primitive types

    go C_Bool     = Dict
    go C_Char     = Dict
    go C_Double   = Dict
    go C_Float    = Dict
    go C_Int      = Dict
    go C_Int16    = Dict
    go C_Int8     = Dict
    go C_Int32    = Dict
    go C_Int64    = Dict
    go C_Integer  = Dict
    go C_Ordering = Dict
    go C_Unit     = Dict
    go C_Word     = Dict
    go C_Word8    = Dict
    go C_Word16   = Dict
    go C_Word32   = Dict
    go C_Word64   = Dict

    -- String types

    go C_String      = Dict
    go C_BS_Strict   = Dict
    go C_BS_Lazy     = Dict
    go C_BS_Short    = Dict
    go C_Text_Strict = Dict
    go C_Text_Lazy   = Dict

    -- Aeson

    go C_Value = Dict

    -- Reference cells

    go C_STRef = Dict
    go C_TVar  = Dict
    go C_MVar  = Dict

    -- Functions

    go C_Fun = Dict

    -- Containers with no type arguments

    go C_IntSet            = Dict
    go C_Prim_ArrayM       = Dict
    go C_Vector_Storable   = Dict
    go C_Vector_StorableM  = Dict
    go C_Vector_Primitive  = Dict
    go C_Vector_PrimitiveM = Dict

{-------------------------------------------------------------------------------
  Compound

  We can't use a type alias for the constraint here as ghc doesn't like
  quantified constraints in constraint type aliases.
-------------------------------------------------------------------------------}

class (
    PrimSatisfies c
    -- Compound
  , forall a.   (c a)      => c (Maybe a)
  , forall a b. (c a, c b) => c (Either a b)
  , forall a.   (c a)      => c [a]
  , forall a.   (c a)      => c (Ratio a)
  , forall a.   (c a)      => c (Set a)
  , forall a b. (c a, c b) => c (Map a b)
  , forall a.   (c a)      => c (IntMap a)
  , forall a.   (c a)      => c (Seq a)
  , forall a.   (c a)      => c (Tree a)
  , forall a.   (c a)      => c (HashSet a)
  , forall a b. (c a, c b) => c (HashMap a b)
  , forall a.   (c a)      => c (HashMap.Array a)
  , forall a.   (c a)      => c (Prim.Array a)
  , forall a.   (c a)      => c (Vector.Boxed.Vector a)
  , forall xs. (All c xs,  IsValidSize (Length xs)) => c (WrappedTuple xs)
  ) => ClassifiedSatisfies (c :: Type -> Constraint)

instance (
    PrimSatisfies c
    -- Compound
  , forall a.   (c a)      => c (Maybe a)
  , forall a b. (c a, c b) => c (Either a b)
  , forall a.   (c a)      => c [a]
  , forall a.   (c a)      => c (Ratio a)
  , forall a.   (c a)      => c (Set a)
  , forall a b. (c a, c b) => c (Map a b)
  , forall a.   (c a)      => c (IntMap a)
  , forall a.   (c a)      => c (Seq a)
  , forall a.   (c a)      => c (Tree a)
  , forall a.   (c a)      => c (HashSet a)
  , forall a b. (c a, c b) => c (HashMap a b)
  , forall a.   (c a)      => c (HashMap.Array a)
  , forall a.   (c a)      => c (Prim.Array a)
  , forall a.   (c a)      => c (Vector.Boxed.Vector a)
  , forall xs. (All c xs,  IsValidSize (Length xs)) => c (WrappedTuple xs)
  ) => ClassifiedSatisfies (c :: Type -> Constraint)

classifiedSatisfies :: forall c o.
     (ClassifiedSatisfies c, c Void)
  => (forall a. o a -> Dict c a)
  -> (forall a. Classifier_ o a -> Dict c a)
classifiedSatisfies otherSatisfies = go
  where
    go :: Classifier_ o a -> Dict c a
    go (C_Prim  c) = primSatisfies  c
    go (C_Other c) = otherSatisfies c

    -- Compound
    go (C_Maybe        c) = goMaybeF     c
    go (C_Either       c) = goEitherF    c
    go (C_List         c) = goMaybeF     c
    go (C_Ratio        c) = goF          c
    go (C_Set          c) = goMaybeF     c
    go (C_Map          c) = goMaybePairF c
    go (C_IntMap       c) = goMaybeF     c
    go (C_Sequence     c) = goMaybeF     c
    go (C_Tree         c) = goF          c
    go (C_HashSet      c) = goF          c
    go (C_HashMap      c) = goMaybePairF c
    go (C_HM_Array     c) = goMaybeF     c
    go (C_Prim_Array   c) = goMaybeF     c
    go (C_Vector_Boxed c) = goMaybeF     c
    go (C_Tuple        c) = goTuple      c

    goF ::
         (forall a. c a => c (f a))
      => (forall a. Classifier_ o a -> Dict c (f a))
    goF c = (\Dict -> Dict) $ go c

    goMaybeF ::
         (forall a. c a => c (f a))
      => (forall a. MaybeF o a -> Dict c (f a))
    goMaybeF FNothing  = Dict
    goMaybeF (FJust c) = (\Dict -> Dict) $ go c

    goEitherF ::
         (forall a b. (c a, c b) => c (f a b))
      => (forall a b. EitherF o a b -> Dict c (f a b))
    goEitherF (FLeft  c) = (\Dict -> Dict) $ go c
    goEitherF (FRight c) = (\Dict -> Dict) $ go c

    goMaybePairF ::
         (forall a b. (c a, c b) => c (f a b))
      => (forall a b. MaybePairF o a b -> Dict c (f a b))
    goMaybePairF FNothingPair      = Dict
    goMaybePairF (FJustPair ca cb) = (\Dict Dict -> Dict) (go ca) (go cb)

    goTuple ::
         (SListI xs, IsValidSize (Length xs))
      => Classifiers o xs
      -> Dict c (WrappedTuple xs)
    goTuple (Classifiers cs) =
         case all_NP (hmap go cs) of
           Dict -> Dict
