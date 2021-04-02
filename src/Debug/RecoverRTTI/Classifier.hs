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
  , Classifiers(..)
    -- * Generalizations
  , Classifier_(..)
    -- * Partial information
  , MaybeF(..)
  , EitherF(..)
  , MaybePairF(..)
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
import qualified Data.ByteString.Short       as BS.Short
import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.Primitive.Array        as Prim (Array)
import qualified Data.Text                   as Text.Strict
import qualified Data.Text.Lazy              as Text.Lazy
import qualified Data.Vector                 as Vector.Boxed

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

  C_Maybe        :: MaybeF      o a   -> Classifier_ o (Maybe a)
  C_Either       :: EitherF     o a b -> Classifier_ o (Either a b)
  C_List         :: MaybeF      o a   -> Classifier_ o [a]
  C_Ratio        :: Classifier_ o a   -> Classifier_ o (Ratio a)
  C_Set          :: MaybeF      o a   -> Classifier_ o (Set a)
  C_Map          :: MaybePairF  o a b -> Classifier_ o (Map a b)
  C_IntMap       :: MaybeF      o a   -> Classifier_ o (IntMap a)
  C_Sequence     :: MaybeF      o a   -> Classifier_ o (Seq a)
  C_Tree         :: Classifier_ o a   -> Classifier_ o (Tree a)
  C_HashSet      :: Classifier_ o a   -> Classifier_ o (HashSet a)
  C_HashMap      :: MaybePairF  o a b -> Classifier_ o (HashMap a b)
  C_HM_Array     :: MaybeF      o a   -> Classifier_ o (HashMap.Array a)
  C_Prim_Array   :: MaybeF      o a   -> Classifier_ o (Prim.Array a)
  C_Vector_Boxed :: MaybeF      o a   -> Classifier_ o (Vector.Boxed.Vector a)

  C_Tuple ::
       (SListI xs, IsValidSize (Length xs))
    => Classifiers o xs -> Classifier_ o (WrappedTuple xs)

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
  C_BS_Short    :: PrimClassifier BS.Short.ShortByteString
  C_Text_Strict :: PrimClassifier Text.Strict.Text
  C_Text_Lazy   :: PrimClassifier Text.Lazy.Text

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

-- | Classifiers for a type with a variable number of arguments
newtype Classifiers o xs = Classifiers {
      getClassifiers :: NP (Classifier_ o) xs
    }

{-------------------------------------------------------------------------------
  Partial information
-------------------------------------------------------------------------------}

data MaybeF o a where
  FNothing :: MaybeF f Void
  FJust    :: Classifier_ o a -> MaybeF o a

data EitherF o a b where
  FLeft  :: Classifier_ o a -> EitherF o a Void
  FRight :: Classifier_ o b -> EitherF o Void b

data MaybePairF o a b where
  FNothingPair :: MaybePairF o Void Void
  FJustPair    :: Classifier_ o a -> Classifier_ o b -> MaybePairF o a b

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

deriving instance Show (PrimClassifier a)

deriving instance (forall x. Show (o x)) => Show (Classifier_ o a)
deriving instance (forall x. Show (o x)) => Show (MaybeF      o a)
deriving instance (forall x. Show (o x)) => Show (EitherF     o a b)
deriving instance (forall x. Show (o x)) => Show (MaybePairF  o a b)

instance (forall a. Show (o a), SListI xs) => Show (Classifiers o xs) where
  showsPrec p (Classifiers xs) =
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

    -- Compound

    go (C_Maybe        c) = C_Maybe        <$> mapMaybeF      c
    go (C_Either       c) = C_Either       <$> mapEitherF     c
    go (C_List         c) = C_List         <$> mapMaybeF      c
    go (C_Ratio        c) = C_Ratio        <$> go             c
    go (C_Set          c) = C_Set          <$> mapMaybeF      c
    go (C_Map          c) = C_Map          <$> mapMaybePairF  c
    go (C_IntMap       c) = C_IntMap       <$> mapMaybeF      c
    go (C_Sequence     c) = C_Sequence     <$> mapMaybeF      c
    go (C_Tree         c) = C_Tree         <$> go             c
    go (C_HashSet      c) = C_HashSet      <$> go             c
    go (C_HashMap      c) = C_HashMap      <$> mapMaybePairF  c
    go (C_HM_Array     c) = C_HM_Array     <$> mapMaybeF      c
    go (C_Prim_Array   c) = C_Prim_Array   <$> mapMaybeF      c
    go (C_Vector_Boxed c) = C_Vector_Boxed <$> mapMaybeF      c
    go (C_Tuple        c) = C_Tuple        <$> mapClassifiers c

    mapMaybeF :: forall a. MaybeF o a -> m (MaybeF o' a)
    mapMaybeF FNothing  = pure FNothing
    mapMaybeF (FJust c) = FJust <$> go c

    mapEitherF :: EitherF o a b -> m (EitherF o' a b)
    mapEitherF (FLeft  c) = FLeft  <$> go c
    mapEitherF (FRight c) = FRight <$> go c

    mapMaybePairF :: MaybePairF o a b -> m (MaybePairF o' a b)
    mapMaybePairF FNothingPair     = pure FNothingPair
    mapMaybePairF (FJustPair c c') = FJustPair <$> go c <*> go c'

    mapClassifiers :: SListI xs => Classifiers o xs -> m (Classifiers o' xs)
    mapClassifiers (Classifiers cs) = Classifiers <$> htraverse' go cs
