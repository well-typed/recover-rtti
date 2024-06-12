{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Debug.RecoverRTTI.Classifier (
    Classifier
  , PrimClassifier(..)
  , IsUserDefined(..)
    -- * Generalizations
  , Classifier_(..)
    -- * Nested classification
  , Elem(..)
  , Elems(..)
    -- * Mapping
  , mapClassifier
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

import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Wrappers

{-------------------------------------------------------------------------------
  Classifier
-------------------------------------------------------------------------------}

-- | Classifier
--
-- Given a value of some unknown type @a@, a @Classifier a@ will tell you what
-- the type of @a@ is. This is similar to a @TypeRep@, but since we recover
-- this information from the heap, we have less accurate type information than
-- @TypeRep@ does.
type Classifier = Classifier_ IsUserDefined

-- | User-defined types
--
-- If we classify a type as user-defined, we pair the classifier with the
-- original value. This means that a @Classifier@ is sufficient information
-- for staged inference by client code that may wish to further classify these
-- types given additional domain knowledge (see also 'reclassify_').
data IsUserDefined a where
  IsUserDefined :: UserDefined -> IsUserDefined UserDefined

instance Show (IsUserDefined a) where
  show (IsUserDefined _) = "IsUserDefined"

{-------------------------------------------------------------------------------
  Generalizations
-------------------------------------------------------------------------------}

-- | Generalization of 'Classifier'
--
-- Type arguments:
--
-- * @o@: Classification of " other " types (not explicitly known to the lib)
--
--   Normally we instantiate this to 'IsUserDefined', classifying all unknown
--   types as 'UserDefined'.
--
-- * @a@: The type we're actually classifying
data Classifier_ (o :: Type -> Type) (a :: Type) :: Type where
  -- Primitive and user-defined types
  C_Prim  :: PrimClassifier a -> Classifier_ o a
  C_Other :: o              a -> Classifier_ o a

  -- Compound
  --
  -- NOTE: C_HashSet requires an argument; 'HashSet' and 'HashMap' cannot be
  -- distinguished from just looking at the heap ('HashSet' is a newtype
  -- around 'HashMap'), and so we classify a 'HashMap' with value type @()@
  -- as a 'HashSet'; however, we can only do this of course if we have at
  -- least one element.

  C_Maybe        :: Elems o '[a]    -> Classifier_ o (Maybe a)
  C_Either       :: Elems o '[a, b] -> Classifier_ o (Either a b)
  C_List         :: Elems o '[a]    -> Classifier_ o [a]
  C_Ratio        :: Elems o '[a]    -> Classifier_ o (Ratio a)
  C_Set          :: Elems o '[a]    -> Classifier_ o (Set a)
  C_Map          :: Elems o '[a, b] -> Classifier_ o (Map a b)
  C_IntMap       :: Elems o '[a]    -> Classifier_ o (IntMap a)
  C_Sequence     :: Elems o '[a]    -> Classifier_ o (Seq a)
  C_Tree         :: Elems o '[a]    -> Classifier_ o (Tree a)
  C_HashSet      :: Elems o '[a]    -> Classifier_ o (HashSet a)
  C_HashMap      :: Elems o '[a, b] -> Classifier_ o (HashMap a b)
  C_HM_Array     :: Elems o '[a]    -> Classifier_ o (HashMap.Array a)
  C_Prim_Array   :: Elems o '[a]    -> Classifier_ o (Prim.Array a)
  C_Vector_Boxed :: Elems o '[a]    -> Classifier_ o (Vector.Boxed.Vector a)

  C_Tuple ::
       (SListI xs, IsValidSize (Length xs))
    => Elems o xs -> Classifier_ o (WrappedTuple xs)

-- | Classifier for primitive types
data PrimClassifier (a :: Type) where
  -- Primitive types

  C_Bool     :: PrimClassifier Bool
  C_Char     :: PrimClassifier Char
  C_Double   :: PrimClassifier Double
  C_Float    :: PrimClassifier Float
  C_Int      :: PrimClassifier Int
  C_Int16    :: PrimClassifier Int16
  C_Int8     :: PrimClassifier Int8
  C_Int32    :: PrimClassifier Int32
  C_Int64    :: PrimClassifier Int64
  C_Integer  :: PrimClassifier Integer
  C_Ordering :: PrimClassifier Ordering
  C_Unit     :: PrimClassifier ()
  C_Word     :: PrimClassifier Word
  C_Word8    :: PrimClassifier Word8
  C_Word16   :: PrimClassifier Word16
  C_Word32   :: PrimClassifier Word32
  C_Word64   :: PrimClassifier Word64

  -- String types
  --
  -- We list @String@ separately, so that we show them properly (rather than
  -- as a list of characters). Of course, empty strings will be inferred as
  -- empty lists instead.

  C_String      :: PrimClassifier String
  C_BS_Strict   :: PrimClassifier BS.Strict.ByteString
  C_BS_Lazy     :: PrimClassifier BS.Lazy.ByteString
  C_Text_Strict :: PrimClassifier Text.Strict.Text
  C_Text_Lazy   :: PrimClassifier Text.Lazy.Text

-- in bytestring 0.12, 'ShortByteStringSource' is a newtype around 'ByteArray'
#if !MIN_VERSION_bytestring(0,12,0)
  C_BS_Short    :: PrimClassifier BS.Short.ShortByteString
#endif

  -- Aeson

  C_Value :: PrimClassifier Value

  -- Reference cells

  C_STRef :: PrimClassifier SomeSTRef
  C_TVar  :: PrimClassifier SomeTVar
  C_MVar  :: PrimClassifier SomeMVar

  -- Functions

  C_Fun :: PrimClassifier SomeFun

  -- Containers with no type arguments
  --
  -- We include mutable containers here, because we currently do not attempt
  -- to peek inside them and hence cannot infer any types for their elements.

  C_IntSet            :: PrimClassifier IntSet
  C_Prim_ArrayM       :: PrimClassifier SomePrimArrayM
  C_Vector_Storable   :: PrimClassifier SomeStorableVector
  C_Vector_StorableM  :: PrimClassifier SomeStorableVectorM
  C_Vector_Primitive  :: PrimClassifier SomePrimitiveVector
  C_Vector_PrimitiveM :: PrimClassifier SomePrimitiveVectorM
  C_ByteArray         :: PrimClassifier Prim.ByteArray
  C_MutableByteArray  :: PrimClassifier SomeMutableByteArray

{-------------------------------------------------------------------------------
  Nested classification
-------------------------------------------------------------------------------}

data Elem o a where
  Elem   :: Classifier_ o a -> Elem o a
  NoElem :: Elem o Void

newtype Elems o xs = Elems (NP (Elem o) xs)

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

deriving instance Show (PrimClassifier a)

deriving instance (forall x. Show (o x)) => Show (Classifier_ o a)
deriving instance (forall x. Show (o x)) => Show (Elem o a)

instance (forall a. Show (o a), SListI xs) => Show (Elems o xs) where
  showsPrec p (Elems xs) =
      case all_NP allShow of
        Dict -> showsPrec p xs
    where
      allShow :: NP (Dict (Compose Show (Elem o))) xs
      allShow = hpure Dict

{-------------------------------------------------------------------------------
  Map over classifiers
-------------------------------------------------------------------------------}

mapClassifier :: forall m o o'.
     Applicative m
  => (forall a. o a -> m (o' a))
  -> (forall a. Classifier_ o a -> m (Classifier_ o' a))
mapClassifier other = go
  where
    go :: forall a. Classifier_ o a -> m (Classifier_ o' a)
    -- Primitive and user-defined types

    go (C_Prim  c) = pure (C_Prim c)
    go (C_Other c) = C_Other <$> other c

    -- Compound

    go (C_Maybe        c) = C_Maybe        <$> goElems c
    go (C_Either       c) = C_Either       <$> goElems c
    go (C_List         c) = C_List         <$> goElems c
    go (C_Ratio        c) = C_Ratio        <$> goElems c
    go (C_Set          c) = C_Set          <$> goElems c
    go (C_Map          c) = C_Map          <$> goElems c
    go (C_IntMap       c) = C_IntMap       <$> goElems c
    go (C_Sequence     c) = C_Sequence     <$> goElems c
    go (C_Tree         c) = C_Tree         <$> goElems c
    go (C_HashSet      c) = C_HashSet      <$> goElems c
    go (C_HashMap      c) = C_HashMap      <$> goElems c
    go (C_HM_Array     c) = C_HM_Array     <$> goElems c
    go (C_Prim_Array   c) = C_Prim_Array   <$> goElems c
    go (C_Vector_Boxed c) = C_Vector_Boxed <$> goElems c
    go (C_Tuple        c) = C_Tuple        <$> goElems c

    goElems :: SListI xs => Elems o xs -> m (Elems o' xs)
    goElems (Elems cs) = Elems <$> htraverse' goElem cs

    goElem :: Elem o a -> m (Elem o' a)
    goElem (Elem c) = Elem <$> go c
    goElem NoElem   = pure NoElem
