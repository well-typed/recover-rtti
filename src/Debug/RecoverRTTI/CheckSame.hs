{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Debug.RecoverRTTI.CheckSame (
    -- * Check if two classifiers are the same
    samePrim
  , sameClassifier_
  , sameElem
  , sameElems
  ) where

import Data.SOP
import Data.Type.Equality

import Debug.RecoverRTTI.Classifier

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
    go (C_Maybe        c) (C_Maybe        c') = sameElems sameOther c c' $ Refl
    go (C_Either       c) (C_Either       c') = sameElems sameOther c c' $ Refl
    go (C_List         c) (C_List         c') = sameElems sameOther c c' $ Refl
    go (C_Ratio        c) (C_Ratio        c') = sameElems sameOther c c' $ Refl
    go (C_Set          c) (C_Set          c') = sameElems sameOther c c' $ Refl
    go (C_Map          c) (C_Map          c') = sameElems sameOther c c' $ Refl
    go (C_IntMap       c) (C_IntMap       c') = sameElems sameOther c c' $ Refl
    go (C_Sequence     c) (C_Sequence     c') = sameElems sameOther c c' $ Refl
    go (C_Tree         c) (C_Tree         c') = sameElems sameOther c c' $ Refl
    go (C_HashSet      c) (C_HashSet      c') = sameElems sameOther c c' $ Refl
    go (C_HashMap      c) (C_HashMap      c') = sameElems sameOther c c' $ Refl
    go (C_HM_Array     c) (C_HM_Array     c') = sameElems sameOther c c' $ Refl
    go (C_Prim_Array   c) (C_Prim_Array   c') = sameElems sameOther c c' $ Refl
    go (C_Vector_Boxed c) (C_Vector_Boxed c') = sameElems sameOther c c' $ Refl
    go (C_Tuple        c) (C_Tuple        c') = sameElems sameOther c c' $ Refl

    -- No match
    go _ _ = Nothing
      where
        _checkAllCases :: Classifier_ o a -> ()
        _checkAllCases = \case
           -- Primitive and user-defined
           C_Prim{}  -> ()
           C_Other{} -> ()

           -- Compound
           C_Maybe{}        -> ()
           C_Either{}       -> ()
           C_List{}         -> ()
           C_Ratio{}        -> ()
           C_Set{}          -> ()
           C_Map{}          -> ()
           C_IntMap{}       -> ()
           C_Sequence{}     -> ()
           C_Tree{}         -> ()
           C_HashSet{}      -> ()
           C_HashMap{}      -> ()
           C_HM_Array{}     -> ()
           C_Prim_Array{}   -> ()
           C_Vector_Boxed{} -> ()
           C_Tuple{}        -> ()

sameElem :: forall o.
     (forall a b. o a -> o b -> Maybe (a :~: b))
  -> (forall a b. Elem o a -> Elem o b -> Maybe (a :~: b))
sameElem sameOther = go
  where
    go :: Elem o a -> Elem o b -> Maybe (a :~: b)
    go NoElem     NoElem   = Just Refl
    go NoElem    (Elem _)  = Nothing
    go (Elem _)   NoElem   = Nothing
    go (Elem ca) (Elem cb) = sameClassifier_ sameOther ca cb

sameElems :: forall o r.
     (forall a b. o a -> o b -> Maybe (a :~: b))
  -> (forall as bs. Elems o as -> Elems o bs -> (as ~ bs => r) -> Maybe r)
sameElems sameOther = go
  where
    go :: Elems o as -> Elems o bs -> (as ~ bs => r) -> Maybe r
    go (Elems Nil)       (Elems Nil)         k = Just k
    go (Elems Nil)       (Elems (_  :* _))   _ = Nothing
    go (Elems (_ :* _))  (Elems Nil)         _ = Nothing
    go (Elems (c :* cs)) (Elems (c' :* cs')) k = do
        Refl <- sameElem sameOther c c'
        go (Elems cs) (Elems cs') k
