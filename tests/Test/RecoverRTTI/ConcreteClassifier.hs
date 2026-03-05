module Test.RecoverRTTI.ConcreteClassifier (
    -- * Concrete classifier
    Concrete(..)
  , ConcreteUser(..)
  , Concretes(..)
  ) where

import Data.Kind
import Data.SOP
import Data.SOP.Dict
import Data.Void
import Data.HashMap.Internal.Array qualified as HashMap (Array)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Primitive.Array qualified as Prim (Array)
import Data.Ratio
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tree (Tree)
import Data.Vector qualified as Vector.Boxed

import Debug.RecoverRTTI

import Test.RecoverRTTI.UserDefined

{-------------------------------------------------------------------------------
  Concrete classifier
-------------------------------------------------------------------------------}

-- | Concrete classifier
--
-- The differences between the \"concrete\" classifier 'Concrete' and the
-- 'Classifier' from the main library are
--
-- * 'Concrete' has explicit cases for user-defined types,
--   whereas `Classifier` merely classifying them as 'UserDefined'`
-- * 'Concrete' does not have any \"deferred\" types
--
-- The constructor names intentionally line up exactly with the main library.
--
-- In "Test.RecoverRRTI.Staged" we show that we can do staged inference,
-- using 'classify' repeatedly to recover /all/ (concrete) type information
-- from the type information returned by 'classify' (/if/ we have full
-- information about which user-defined types we're interested in).
data Concrete (a :: Type) :: Type where
  -- Primitive and user-defined types
  CC_Prim  :: PrimClassifier a -> Concrete a
  CC_Other :: ConcreteUser   a -> Concrete a

  -- Void
  --
  -- We use this when generating elements of empty types
  CC_Void  :: Concrete Void

  -- Compound types with unclassified elements in 'Classifier'
  CC_HashSet      :: Concrete a -> Concrete (HashSet a)
  CC_IntMap       :: Concrete a -> Concrete (IntMap  a)
  CC_Maybe        :: Concrete a -> Concrete (Maybe   a)
  CC_Ratio        :: Concrete a -> Concrete (Ratio   a)
  CC_Set          :: Concrete a -> Concrete (Set     a)
  CC_Tree         :: Concrete a -> Concrete (Tree    a)

  CC_HM_Array     :: Concrete a -> Concrete (HashMap.Array a)
  CC_List         :: Concrete a -> Concrete [a]
  CC_Prim_Array   :: Concrete a -> Concrete (Prim.Array a)
  CC_Sequence     :: Concrete a -> Concrete (Seq a)
  CC_Vector_Boxed :: Concrete a -> Concrete (Vector.Boxed.Vector a)

  CC_Either       :: Concrete a -> Concrete b -> Concrete (Either a b)
  CC_HashMap      :: Concrete a -> Concrete b -> Concrete (HashMap a b)
  CC_Map          :: Concrete a -> Concrete b -> Concrete (Map a b)

  -- Compound types which have classified elements even in 'Classifier'

  CC_Tuple ::
       (SListI xs, IsValidSize (Length xs))
    => Concretes xs -> Concrete (WrappedTuple xs)

newtype Concretes xs = Concretes (NP Concrete xs)

-- | Example user-defined types
data ConcreteUser (a :: Type) where
  CC_Simple   :: ConcreteUser SimpleType
  CC_NonRec   :: Concrete a -> ConcreteUser (NonRecursive a)
  CC_Rec      :: Concrete a -> ConcreteUser (Recursive    a)
  CC_Unlifted :: ConcreteUser ContainsUnlifted

deriving instance Show (Concrete a)
deriving instance Show (ConcreteUser a)

instance SListI xs => Show (Concretes xs) where
  showsPrec p (Concretes xs) =
      case all_NP allShow of
        Dict -> showsPrec p xs
    where
      allShow :: NP (Dict (Compose Show Concrete)) xs
      allShow = hpure Dict
