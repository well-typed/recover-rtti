{-# LANGUAGE CPP #-}

module Debug.RecoverRTTI.Classifier (
    Classifier
  , PrimClassifier(..)
  , IsUserDefined(..)
    -- * Generalizations
  , Classifier_(..)
  , ClassifyListElem(..)
  , Classifiers_(..)
    -- * Mapping
  , mapClassifier
  ) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.HashMap.Internal.Array qualified as HashMap (Array)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Kind
import Data.Map (Map)
import Data.Primitive.Array qualified as Prim (Array)
import Data.Primitive.ByteArray qualified as Prim (ByteArray)
import Data.Ratio
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.SOP
import Data.SOP.Dict
import Data.Text qualified as Text.Strict
import Data.Text.Lazy qualified as Text.Lazy
import Data.Tree (Tree)
import Data.Vector qualified as Vector.Boxed
import Data.Word

#if !MIN_VERSION_bytestring(0,12,0)
import Data.ByteString.Short qualified as BS.Short
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
--
-- For containers only the outer shape is inferred; for example, a value of
-- type @Maybe Int@ will be classified as @C_Maybe@, implying it is of type
-- @Maybe Deferred@. Specific applications, such as 'anythingToString', then
-- depend on recursive classification for the elements.
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

  -- Compound types with unclassified elements

  C_HashSet      :: Classifier_ o (HashSet Deferred)
  C_IntMap       :: Classifier_ o (IntMap Deferred)
  C_Maybe        :: Classifier_ o (Maybe Deferred)
  C_Ratio        :: Classifier_ o (Ratio Deferred)
  C_Set          :: Classifier_ o (Set Deferred)
  C_Tree         :: Classifier_ o (Tree Deferred)

  C_HM_Array     :: ClassifyListElem a -> Classifier_ o (HashMap.Array a)
  C_List         :: ClassifyListElem a -> Classifier_ o [a]
  C_Prim_Array   :: ClassifyListElem a -> Classifier_ o (Prim.Array a)
  C_Sequence     :: ClassifyListElem a -> Classifier_ o (Seq a)
  C_Vector_Boxed :: ClassifyListElem a -> Classifier_ o (Vector.Boxed.Vector a)

  C_Either       :: Classifier_ o (Either Deferred Deferred)
  C_HashMap      :: Classifier_ o (HashMap Deferred Deferred)
  C_Map          :: Classifier_ o (Map Deferred Deferred)

  -- Compound types with classified elements
  --
  -- We should infer type arguments /only/ if there is exactly one use of that
  -- type variable in values; in all other cases we might infer something based
  -- on the first value wihich might not be true for the other values, and
  -- therefore we should instead defer.

  C_Tuple ::
       (SListI xs, IsValidSize (Length xs))
    => Classifiers_ o xs -> Classifier_ o (WrappedTuple xs)

-- | Distinguish lists of characters from other lists
--
-- This ensures that we print strings as strings, rather than lists of chars.
data ClassifyListElem (a :: Type) where
  C_List_Deferred :: ClassifyListElem Deferred
  C_List_Char     :: ClassifyListElem Char

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

newtype Classifiers_ o xs = Classifiers_ (NP (Classifier_ o) xs)

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

deriving instance Show (PrimClassifier   a)
deriving instance Show (ClassifyListElem a)

deriving instance (forall x. Show (o x)) => Show (Classifier_ o a)

instance (forall a. Show (o a), SListI xs) => Show (Classifiers_ o xs) where
  showsPrec p (Classifiers_ xs) =
      case all_NP allShow of
        Dict -> showsPrec p xs
    where
      allShow :: NP (Dict (Compose Show (Classifier_ o))) xs
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

    -- Compound types with unclassified elements
    go C_HashSet          = pure C_HashSet
    go C_IntMap           = pure C_IntMap
    go C_Maybe            = pure C_Maybe
    go C_Ratio            = pure C_Ratio
    go C_Set              = pure C_Set
    go C_Tree             = pure C_Tree

    go (C_HM_Array     c) = pure (C_HM_Array     c)
    go (C_List         c) = pure (C_List         c)
    go (C_Prim_Array   c) = pure (C_Prim_Array   c)
    go (C_Sequence     c) = pure (C_Sequence     c)
    go (C_Vector_Boxed c) = pure (C_Vector_Boxed c)

    go C_Either           = pure C_Either
    go C_HashMap          = pure C_HashMap
    go C_Map              = pure C_Map

    -- Compound types with classified elements
    go (C_Tuple cs) = C_Tuple <$> goNP cs

    goNP :: SListI xs => Classifiers_ o xs -> m (Classifiers_ o' xs)
    goNP (Classifiers_ cs) = Classifiers_ <$> htraverse' go cs