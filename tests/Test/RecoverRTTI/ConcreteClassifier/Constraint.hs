module Test.RecoverRTTI.ConcreteClassifier.Constraint (
    UserSatisfies
  , userSatisfies
  , concreteSatisfies
  , canShowConcrete
  , canCompareConcrete
  ) where

import Data.HashMap.Internal.Array qualified as HashMap (Array)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.Kind
import Data.Map (Map)
import Data.Primitive.Array qualified as Prim (Array)
import Data.Ratio
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.SOP
import Data.SOP.Dict
import Data.Tree (Tree)
import Data.Vector qualified as Vector.Boxed
import Data.Void

import Debug.RecoverRTTI (PrimSatisfies, primSatisfies)
import Debug.RecoverRTTI (IsValidSize, Length, WrappedTuple)

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.Orphans ()
import Test.RecoverRTTI.UserDefined

{-------------------------------------------------------------------------------
  User-defined types
-------------------------------------------------------------------------------}

class (
    c SimpleType
  , forall a. c a => c (NonRecursive a)
  , forall a. c a => c (Recursive    a)
  , c ContainsUnlifted
  ) => UserSatisfies c

instance (
    c SimpleType
  , forall a. c a => c (NonRecursive a)
  , forall a. c a => c (Recursive    a)
  , c ContainsUnlifted
  ) => UserSatisfies c

userSatisfies :: forall c a. ConcreteSatisfies c => ConcreteUser a -> Dict c a
userSatisfies = go
  where
    go :: forall x. ConcreteUser x -> Dict c x
    go  CC_Simple    = Dict
    go (CC_NonRec c) = case auxConcrete c of Dict -> Dict
    go (CC_Rec    c) = case auxConcrete c of Dict -> Dict
    go  CC_Unlifted  = Dict

    auxConcrete :: forall x. Concrete x -> Dict c x
    auxConcrete = concreteSatisfies

{-------------------------------------------------------------------------------
  Compound
-------------------------------------------------------------------------------}

class (
    PrimSatisfies c
  , UserSatisfies c
  , c Void
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
  ) => ConcreteSatisfies (c :: Type -> Constraint)

instance (
    PrimSatisfies c
  , UserSatisfies c
  , c Void
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
  ) => ConcreteSatisfies (c :: Type -> Constraint)

concreteSatisfies :: forall c a. ConcreteSatisfies c => Concrete a -> Dict c a
concreteSatisfies = go
  where
    go :: forall x. Concrete x -> Dict c x
    go (CC_Prim  c) = primSatisfies  c
    go (CC_Other c) = userSatisfies c
    go  CC_Void     = Dict

   -- Compound types with unclassified elements
    go (CC_HashSet      c1   ) = case go c1 of Dict -> Dict
    go (CC_IntMap       c1   ) = case go c1 of Dict -> Dict
    go (CC_Maybe        c1   ) = case go c1 of Dict -> Dict
    go (CC_Ratio        c1   ) = case go c1 of Dict -> Dict
    go (CC_Set          c1   ) = case go c1 of Dict -> Dict
    go (CC_Tree         c1   ) = case go c1 of Dict -> Dict

    go (CC_HM_Array     c1   ) = case go c1 of Dict -> Dict
    go (CC_List         c1   ) = case go c1 of Dict -> Dict
    go (CC_Prim_Array   c1   ) = case go c1 of Dict -> Dict
    go (CC_Sequence     c1   ) = case go c1 of Dict -> Dict
    go (CC_Vector_Boxed c1   ) = case go c1 of Dict -> Dict

    go (CC_Either       c1 c2) = case (go c1, go c2) of (Dict, Dict) -> Dict
    go (CC_HashMap      c1 c2) = case (go c1, go c2) of (Dict, Dict) -> Dict
    go (CC_Map          c1 c2) = case (go c1, go c2) of (Dict, Dict) -> Dict

    -- Compound types with classified elements
    go (CC_Tuple cs) = goNP cs Dict

    goNP :: SListI as => Concretes as -> (All c as => r) -> r
    goNP (Concretes cs) k = case all_NP (hmap go cs) of Dict -> k

canShowConcrete :: Concrete a -> Dict Show a
canShowConcrete = concreteSatisfies

canCompareConcrete :: Concrete a -> Dict Eq a
canCompareConcrete = concreteSatisfies
