{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module Debug.RecoverRTTI.CheckSame (
    -- * Check if two classifiers are the same
    samePrim
  , sameClassifier_
  ) where

import Data.SOP
import Data.Type.Equality

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Tuple

{-------------------------------------------------------------------------------
  Equality check
-------------------------------------------------------------------------------}

samePrim :: PrimClassifier a -> PrimClassifier b -> Maybe (a :~: b)
samePrim = go
  where
    go :: PrimClassifier a -> PrimClassifier b -> Maybe (a :~: b)

    -- Primitive types
    go C_Bool     C_Bool     = Just Refl
    go C_Char     C_Char     = Just Refl
    go C_Double   C_Double   = Just Refl
    go C_Float    C_Float    = Just Refl
    go C_Int      C_Int      = Just Refl
    go C_Int8     C_Int8     = Just Refl
    go C_Int16    C_Int16    = Just Refl
    go C_Int32    C_Int32    = Just Refl
    go C_Int64    C_Int64    = Just Refl
    go C_Integer  C_Integer  = Just Refl
    go C_Ordering C_Ordering = Just Refl
    go C_Unit     C_Unit     = Just Refl
    go C_Word     C_Word     = Just Refl
    go C_Word8    C_Word8    = Just Refl
    go C_Word16   C_Word16   = Just Refl
    go C_Word32   C_Word32   = Just Refl
    go C_Word64   C_Word64   = Just Refl

    -- String types

    go C_String      C_String      = Just Refl
    go C_BS_Strict   C_BS_Strict   = Just Refl
    go C_BS_Lazy     C_BS_Lazy     = Just Refl
    go C_BS_Short    C_BS_Short    = Just Refl
    go C_Text_Strict C_Text_Strict = Just Refl
    go C_Text_Lazy   C_Text_Lazy   = Just Refl

    -- Aeson

    go C_Value C_Value = Just Refl

    -- Reference cells

    go C_STRef C_STRef = Just Refl
    go C_TVar  C_TVar  = Just Refl
    go C_MVar  C_MVar  = Just Refl

    -- Containers without type arguments

    go C_IntSet            C_IntSet            = Just Refl
    go C_Prim_ArrayM       C_Prim_ArrayM       = Just Refl
    go C_Vector_Storable   C_Vector_Storable   = Just Refl
    go C_Vector_StorableM  C_Vector_StorableM  = Just Refl
    go C_Vector_Primitive  C_Vector_Primitive  = Just Refl
    go C_Vector_PrimitiveM C_Vector_PrimitiveM = Just Refl

    -- Functions

    go C_Fun C_Fun = Just Refl

    -- Not equal
    go _ _ = Nothing

    _checkAllCases :: PrimClassifier a -> ()
    _checkAllCases = \case
        -- Primitive types

        C_Bool     -> ()
        C_Char     -> ()
        C_Double   -> ()
        C_Float    -> ()
        C_Int      -> ()
        C_Int8     -> ()
        C_Int16    -> ()
        C_Int32    -> ()
        C_Int64    -> ()
        C_Integer  -> ()
        C_Ordering -> ()
        C_Unit     -> ()
        C_Word     -> ()
        C_Word8    -> ()
        C_Word16   -> ()
        C_Word32   -> ()
        C_Word64   -> ()

        -- String types

        C_String      -> ()
        C_BS_Strict   -> ()
        C_BS_Lazy     -> ()
        C_BS_Short    -> ()
        C_Text_Strict -> ()
        C_Text_Lazy   -> ()

        -- Aeson

        C_Value -> ()

        -- Reference cells

        C_STRef -> ()
        C_TVar  -> ()
        C_MVar  -> ()

        -- Containers without type arguments

        C_IntSet            -> ()
        C_Prim_ArrayM       -> ()
        C_Vector_Storable   -> ()
        C_Vector_StorableM  -> ()
        C_Vector_Primitive  -> ()
        C_Vector_PrimitiveM -> ()

        -- Functions

        C_Fun -> ()

-- | Check that two classifiers are the same
--
-- If they are the same, additionally return a proof that that means the
-- /types/ they classify must be equal (note that equality on the classifiers
-- is strictly stronger than equality on the types: for example, non-empty
-- and empty lists have different classifiers, but classify the same type).
--
-- This is defined on the general type 'Classifier_' rather than on 'Classifier'
-- because different user-defined types may both be classified as @UserDefined@
-- yet not be equal to each other
sameClassifier_ :: forall o.
     (forall a b. o a -> o b -> Maybe (a :~: b))
  -> (forall a b. Classifier_ o a -> Classifier_ o b -> Maybe (a :~: b))
sameClassifier_ sameOther = go
  where
    go :: Classifier_ o a -> Classifier_ o b -> Maybe (a :~: b)

    -- User-defined and primitive types
    go (C_Prim  c) (C_Prim  c') = samePrim  c c'
    go (C_Other c) (C_Other c') = sameOther c c'

    -- Compound
    go (C_Maybe           c) (C_Maybe           c') = goMaybeF     c c'
    go (C_Either          c) (C_Either          c') = goEitherF    c c'
    go (C_List            c) (C_List            c') = goMaybeF     c c'
    go (C_Ratio           c) (C_Ratio           c') = goF          c c'
    go (C_Set             c) (C_Set             c') = goMaybeF     c c'
    go (C_Map             c) (C_Map             c') = goMaybePairF c c'
    go (C_IntMap          c) (C_IntMap          c') = goMaybeF     c c'
    go (C_Sequence        c) (C_Sequence        c') = goMaybeF     c c'
    go (C_Tree            c) (C_Tree            c') = goF          c c'
    go (C_HashSet         c) (C_HashSet         c') = goF          c c'
    go (C_HashMap         c) (C_HashMap         c') = goMaybePairF c c'
    go (C_HM_Array        c) (C_HM_Array        c') = goMaybeF     c c'
    go (C_Prim_Array      c) (C_Prim_Array      c') = goMaybeF     c c'
    go (C_Vector_Boxed    c) (C_Vector_Boxed    c') = goMaybeF     c c'
    go (C_Vector_Unboxed  c) (C_Vector_Unboxed  c') = goClosedF    c c'
    go (C_Vector_UnboxedM c) (C_Vector_UnboxedM c') = goClosedF    c c'
    go (C_Tuple           c) (C_Tuple           c') = goTuple      c c'

    -- No match
    go _ _ = Nothing

      where
        _checkAllCases :: Classifier_ o a -> ()
        _checkAllCases = \case
           -- Primitive and user-defined
           C_Prim{}  -> ()
           C_Other{} -> ()

           -- Compound
           C_Maybe{}           -> ()
           C_Either{}          -> ()
           C_List{}            -> ()
           C_Ratio{}           -> ()
           C_Set{}             -> ()
           C_Map{}             -> ()
           C_IntMap{}          -> ()
           C_Sequence{}        -> ()
           C_Tree{}            -> ()
           C_HashSet{}         -> ()
           C_HashMap{}         -> ()
           C_HM_Array{}        -> ()
           C_Prim_Array{}      -> ()
           C_Vector_Boxed{}    -> ()
           C_Vector_Unboxed{}  -> ()
           C_Vector_UnboxedM{} -> ()
           C_Tuple{}           -> ()

    goF :: Classifier_ o a -> Classifier_ o b -> Maybe (f a :~: f b)
    goF a b = (\Refl -> Refl) <$> go a b

    goClosedF :: Classifier_ C a -> Classifier_ C b -> Maybe (f a :~: f b)
    goClosedF a b = (\Refl -> Refl) <$> sameClassifier_ noOtherTypes a b
      where
        noOtherTypes :: C a -> C b -> Maybe (a :~: b)
        noOtherTypes x = case x of {}

    goMaybeF :: MaybeF o a -> MaybeF o b -> Maybe (f a :~: f b)
    goMaybeF FNothing  FNothing  = Just Refl
    goMaybeF (FJust a) (FJust b) = (\Refl -> Refl) <$> go a b
    goMaybeF _         _         = Nothing

    goEitherF :: EitherF o a a' -> EitherF o b b' -> Maybe (f a a' :~: f b b')
    goEitherF (FLeft  a) (FLeft  b) = (\Refl -> Refl) <$> go a b
    goEitherF (FRight a) (FRight b) = (\Refl -> Refl) <$> go a b
    goEitherF _          _          = Nothing

    goMaybePairF ::
         MaybePairF o a a'
      -> MaybePairF o b b'
      -> Maybe (f a a' :~: f b b')
    goMaybePairF FNothingPair      FNothingPair    = Just Refl
    goMaybePairF (FJustPair a a') (FJustPair b b') = (\Refl Refl -> Refl)
                                                         <$> go a  b
                                                         <*> go a' b'
    goMaybePairF _                 _                 = Nothing

    goTuple ::
         Classifiers o xs
      -> Classifiers o ys
      -> Maybe (WrappedTuple xs :~: WrappedTuple ys)
    goTuple = \(Classifiers xs) (Classifiers ys) -> aux xs ys
      where
        aux :: NP (Classifier_ o) xs
            -> NP (Classifier_ o) ys
            -> Maybe (WrappedTuple xs :~: WrappedTuple ys)
        aux Nil       Nil       = Just Refl
        aux (x :* xs) (y :* ys) = (\Refl Refl -> Refl)
                                    <$> go x y
                                    <*> aux xs ys
        aux _         _         = Nothing
