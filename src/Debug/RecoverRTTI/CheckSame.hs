{-# LANGUAGE CPP #-}

module Debug.RecoverRTTI.CheckSame (
    -- * Check if two classifiers are the same
    samePrim
  , sameClassifier_
  , sameClassifiers_
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

    go C_BS_Strict   C_BS_Strict   = Just Refl
    go C_BS_Lazy     C_BS_Lazy     = Just Refl
    go C_Text_Strict C_Text_Strict = Just Refl
    go C_Text_Lazy   C_Text_Lazy   = Just Refl

#if !MIN_VERSION_bytestring(0,12,0)
    go C_BS_Short    C_BS_Short    = Just Refl
#endif

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
    go C_ByteArray         C_ByteArray         = Just Refl
    go C_MutableByteArray  C_MutableByteArray  = Just Refl

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

        C_BS_Strict   -> ()
        C_BS_Lazy     -> ()
        C_Text_Strict -> ()
        C_Text_Lazy   -> ()

#if !MIN_VERSION_bytestring(0,12,0)
        C_BS_Short    -> ()
#endif

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
        C_ByteArray         -> ()
        C_MutableByteArray  -> ()

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

    -- Compound types with unclassified elements
    go C_Maybe            C_Maybe             = Just Refl
    go C_Ratio            C_Ratio             = Just Refl
    go C_Set              C_Set               = Just Refl
    go C_IntMap           C_IntMap            = Just Refl
    go C_Tree             C_Tree              = Just Refl
    go C_HashSet          C_HashSet           = Just Refl

    go (C_List         c) (C_List         c') = listElem c c'
    go (C_Sequence     c) (C_Sequence     c') = listElem c c'
    go (C_HM_Array     c) (C_HM_Array     c') = listElem c c'
    go (C_Prim_Array   c) (C_Prim_Array   c') = listElem c c'
    go (C_Vector_Boxed c) (C_Vector_Boxed c') = listElem c c'

    go C_Either           C_Either            = Just Refl
    go C_Map              C_Map               = Just Refl
    go C_HashMap          C_HashMap           = Just Refl

    -- Compound types with classified elements
    go (C_Tuple cs) (C_Tuple cs') = sameClassifiers_ sameOther cs cs' $ Refl

    -- No match
    go _ _ = Nothing
      where
        _checkAllCases :: Classifier_ o a -> ()
        _checkAllCases = \case
           -- Primitive and user-defined
           C_Prim{}  -> ()
           C_Other{} -> ()

           -- Compound
           C_Either{}       -> ()
           C_HashMap{}      -> ()
           C_HashSet{}      -> ()
           C_HM_Array{}     -> ()
           C_IntMap{}       -> ()
           C_List{}         -> ()
           C_Map{}          -> ()
           C_Maybe{}        -> ()
           C_Prim_Array{}   -> ()
           C_Ratio{}        -> ()
           C_Sequence{}     -> ()
           C_Set{}          -> ()
           C_Tree{}         -> ()
           C_Tuple{}        -> ()
           C_Vector_Boxed{} -> ()

    listElem :: ClassifyListElem a -> ClassifyListElem b -> Maybe (f a :~: f b)
    listElem C_List_Deferred C_List_Deferred = Just Refl
    listElem C_List_Char     C_List_Char     = Just Refl
    listElem _               _               = Nothing

sameClassifiers_ :: forall o r.
     ( forall a b. o a -> o b -> Maybe (a :~: b) )
  -> ( forall as bs.
            Classifiers_ o as
         -> Classifiers_ o bs
         -> (as ~ bs => r)
         -> Maybe r
     )
sameClassifiers_ sameOther = \(Classifiers_ cs) (Classifiers_ cs') ->
    go cs cs'
  where
    go ::
         NP (Classifier_ o) as
      -> NP (Classifier_ o) bs
      -> (as ~ bs => r)
      -> Maybe r
    go Nil       Nil         k = Just k
    go Nil       (_  :* _)   _ = Nothing
    go (_ :* _)  Nil         _ = Nothing
    go (c :* cs) (c' :* cs') k = do
        Refl <- sameClassifier_ sameOther c c'
        go cs cs' k
