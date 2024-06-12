{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
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
import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.Primitive.Array        as Prim (Array)
import qualified Data.Primitive.ByteArray    as Prim (ByteArray)
import qualified Data.Text                   as Text.Strict
import qualified Data.Text.Lazy              as Text.Lazy
import qualified Data.Vector                 as Vector.Boxed

#if !MIN_VERSION_bytestring(0,12,0)
import qualified Data.ByteString.Short as BS.Short
#endif

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
  , c Text.Strict.Text
  , c Text.Lazy.Text

#if !MIN_VERSION_bytestring(0,12,0)
  , c BS.Short.ShortByteString
#endif

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
  , c Prim.ByteArray
  , c SomeMutableByteArray
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
    go C_Text_Strict = Dict
    go C_Text_Lazy   = Dict

#if !MIN_VERSION_bytestring(0,12,0)
    go C_BS_Short    = Dict
#endif

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
    go C_ByteArray         = Dict
    go C_MutableByteArray  = Dict

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
    go (C_Maybe        c) = goElems c $ Dict
    go (C_Either       c) = goElems c $ Dict
    go (C_List         c) = goElems c $ Dict
    go (C_Ratio        c) = goElems c $ Dict
    go (C_Set          c) = goElems c $ Dict
    go (C_Map          c) = goElems c $ Dict
    go (C_IntMap       c) = goElems c $ Dict
    go (C_Sequence     c) = goElems c $ Dict
    go (C_Tree         c) = goElems c $ Dict
    go (C_HashSet      c) = goElems c $ Dict
    go (C_HashMap      c) = goElems c $ Dict
    go (C_HM_Array     c) = goElems c $ Dict
    go (C_Prim_Array   c) = goElems c $ Dict
    go (C_Vector_Boxed c) = goElems c $ Dict
    go (C_Tuple        c) = goElems c $ Dict

    goElems :: SListI as => Elems o as -> (All c as => r) -> r
    goElems (Elems cs) k = case all_NP (hmap goElem cs) of Dict -> k

    goElem :: Elem o a -> Dict c a
    goElem (Elem c) = go c
    goElem NoElem   = Dict
