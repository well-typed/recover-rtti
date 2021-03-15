{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Debug.RecoverRTTI.Classifier (
    Classifier(..)
  , Classifiers(..)
  , Classified(..)
    -- * Partial information
  , MaybeF(..)
  , EitherF(..)
  , MaybePairF(..)
  ) where

import Data.Aeson (Value)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Kind
import Data.Map (Map)
import Data.Ratio
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.SOP
import Data.Tree (Tree)
import Data.Void
import Data.Word

import qualified Data.ByteString       as BS.Strict
import qualified Data.ByteString.Lazy  as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import qualified Data.Text             as Text.Strict
import qualified Data.Text.Lazy        as Text.Lazy

import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.TypeLevel
import Debug.RecoverRTTI.UserDefined
import Debug.RecoverRTTI.Wrappers

{-------------------------------------------------------------------------------
  Classifier
-------------------------------------------------------------------------------}

-- | A value along with its classifier
data Classified a = Classified {
      classifiedType  :: Classifier a
    , classifiedValue :: a
    }

-- | Classifier
--
-- Given a value of some unknown type @a@, a @Classifier a@ will tell you what
-- the type of @a@ is. This is similar to a @TypeRep@, but since we recover
-- this information from the heap, we have less accurate type information than
-- @TypeRep@ does.
data Classifier (a :: Type) :: Type where
  -- Primitive types

  C_Bool     :: Classifier Bool
  C_Char     :: Classifier Char
  C_Double   :: Classifier Double
  C_Float    :: Classifier Float
  C_Int      :: Classifier Int
  C_Int16    :: Classifier Int16
  C_Int8     :: Classifier Int8
  C_Int32    :: Classifier Int32
  C_Int64    :: Classifier Int64
  C_Integer  :: Classifier Integer
  C_Ordering :: Classifier Ordering
  C_Unit     :: Classifier ()
  C_Word     :: Classifier Word
  C_Word8    :: Classifier Word8
  C_Word16   :: Classifier Word16
  C_Word32   :: Classifier Word32
  C_Word64   :: Classifier Word64

  -- String types
  --
  -- We list @String@ separately, so that we show them properly (rather than
  -- as a list of characters). Of course, empty strings will be inferred as
  -- empty lists instead.

  C_String      :: Classifier String
  C_BS_Strict   :: Classifier BS.Strict.ByteString
  C_BS_Lazy     :: Classifier BS.Lazy.ByteString
  C_BS_Short    :: Classifier BS.Short.ShortByteString
  C_Text_Strict :: Classifier Text.Strict.Text
  C_Text_Lazy   :: Classifier Text.Lazy.Text

  -- Aeson

  C_Value :: Classifier Value

  -- Compound

  C_Maybe    :: MaybeF     Classified a   -> Classifier (Maybe a)
  C_Either   :: EitherF    Classified a b -> Classifier (Either a b)
  C_List     :: MaybeF     Classified a   -> Classifier [a]
  C_Ratio    ::            Classified a   -> Classifier (Ratio a)
  C_Set      :: MaybeF     Classified a   -> Classifier (Set a)
  C_Map      :: MaybePairF Classified a b -> Classifier (Map a b)
  C_IntSet   ::                              Classifier IntSet
  C_IntMap   :: MaybeF     Classified a   -> Classifier (IntMap a)
  C_Sequence :: MaybeF     Classified a   -> Classifier (Seq a)
  C_Tree     ::            Classified a   -> Classifier (Tree a)

  C_Tuple ::
       (SListI xs, IsValidSize (Length xs))
    => Classifiers xs -> Classifier (WrappedTuple xs)

  -- Reference cells

  C_STRef :: Classifier SomeSTRef
  C_TVar  :: Classifier SomeTVar
  C_MVar  :: Classifier SomeMVar

  -- Functions

  C_Fun :: Classifier SomeFun

  -- User-defined

  C_Custom :: Sing c -> Classifier (UserDefined c)

newtype Classifiers xs = Classifiers (NP Classified xs)

{-------------------------------------------------------------------------------
  Partial information
-------------------------------------------------------------------------------}

data MaybeF f a where
  FNothing :: MaybeF f Void
  FJust    :: f a -> MaybeF f a

data EitherF f a b where
  FLeft  :: f a -> EitherF f a Void
  FRight :: f b -> EitherF f Void b

data MaybePairF f a b where
  FNothingPair :: MaybePairF f Void Void
  FJustPair    :: f a -> f b -> MaybePairF f a b
