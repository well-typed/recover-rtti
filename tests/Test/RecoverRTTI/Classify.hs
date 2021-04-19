{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Verify we infer the right classifier
module Test.RecoverRTTI.Classify (tests) where

import Control.Monad.Except
import Data.Ratio
import Data.SOP
import Data.Type.Equality
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Aeson                  as Aeson
import qualified Data.HashMap.Internal.Array as HashMap.Array
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.HashSet                as HashSet
import qualified Data.IntMap                 as IntMap
import qualified Data.IntSet                 as IntSet
import qualified Data.Map                    as Map
import qualified Data.Primitive.Array        as Prim.Array
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Tree                   as Tree
import qualified Data.Vector                 as Vector.Boxed
import qualified Data.Vector.Storable        as Vector.Storable
import qualified Data.Vector.Primitive       as Vector.Primitive

import Test.Tasty
import Test.Tasty.QuickCheck hiding (classify, NonEmpty)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Classify

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.Globals
import Test.RecoverRTTI.Staged
import Test.RecoverRTTI.UserDefined

tests :: TestTree
tests = testGroup "Test.RecoverRTTI.Classify" [
      testProperty "constants" prop_constants
    , testProperty "arbitrary" prop_arbitrary
    ]

-- | Test using manually specified examples
--
-- For " normal " code it doesn't matter if something is generated or not,
-- but their on-heap representation may be different, and this may effect the
-- RTTI recovery.
prop_constants :: Property
prop_constants = withMaxSuccess 1 $ conjoin [
      -- Primitive types

      compareClassifier $ Value (C_Prim C_Bool)     True
    , compareClassifier $ Value (C_Prim C_Bool)     False
    , compareClassifier $ Value (C_Prim C_Char)     'a'
    , compareClassifier $ Value (C_Prim C_Double)   1.25
    , compareClassifier $ Value (C_Prim C_Float)    1.25
    , compareClassifier $ Value (C_Prim C_Int)      1234
    , compareClassifier $ Value (C_Prim C_Int)      (-1234)
    , compareClassifier $ Value (C_Prim C_Int8)     123
    , compareClassifier $ Value (C_Prim C_Int16)    1234
    , compareClassifier $ Value (C_Prim C_Int32)    1234
    , compareClassifier $ Value (C_Prim C_Int64)    1234
    , compareClassifier $ Value (C_Prim C_Integer)  1234
    , compareClassifier $ Value (C_Prim C_Integer)  (succ (fromIntegral (maxBound :: Int)))
    , compareClassifier $ Value (C_Prim C_Integer)  (pred (fromIntegral (minBound :: Int)))
    , compareClassifier $ Value (C_Prim C_Ordering) LT
    , compareClassifier $ Value (C_Prim C_Ordering) GT
    , compareClassifier $ Value (C_Prim C_Ordering) EQ
    , compareClassifier $ Value (C_Prim C_Unit)     ()
    , compareClassifier $ Value (C_Prim C_Word)     1234
    , compareClassifier $ Value (C_Prim C_Word8)    123
    , compareClassifier $ Value (C_Prim C_Word16)   134
    , compareClassifier $ Value (C_Prim C_Word32)   1234
    , compareClassifier $ Value (C_Prim C_Word64)   1234

      -- String types
      --
      -- We skip the empty string, because we infer that as @C_List Empty@

    , compareClassifier $ Value (C_Prim C_String)      "abcdefg"
    , compareClassifier $ Value (C_Prim C_BS_Strict)   ""
    , compareClassifier $ Value (C_Prim C_BS_Strict)   "abcdefg"
    , compareClassifier $ Value (C_Prim C_BS_Lazy)     ""
    , compareClassifier $ Value (C_Prim C_BS_Lazy)     "abcdefg"
    , compareClassifier $ Value (C_Prim C_BS_Short)    ""
    , compareClassifier $ Value (C_Prim C_BS_Short)    "abcdefg"
    , compareClassifier $ Value (C_Prim C_Text_Strict) ""
    , compareClassifier $ Value (C_Prim C_Text_Strict) "abcdefg"
    , compareClassifier $ Value (C_Prim C_Text_Lazy)   ""
    , compareClassifier $ Value (C_Prim C_Text_Lazy)   "abcdefg"

      -- Aeson

    , compareClassifier $ Value (C_Prim C_Value) (Aeson.object [("x" Aeson..= True)])

      -- Reference cells

    , compareClassifier $ Value (C_Prim C_STRef) exampleIORef
    , compareClassifier $ Value (C_Prim C_STRef) exampleSTRef
    , compareClassifier $ Value (C_Prim C_MVar)  exampleMVar
    , compareClassifier $ Value (C_Prim C_TVar)  exampleTVar

      -- Functions

    , compareClassifier $ Value (C_Prim C_Fun) (SomeFun id)

      -- Containers without type arguments

    , compareClassifier $ Value (C_Prim C_IntSet) $
        IntSet.empty
    , compareClassifier $ Value (C_Prim C_IntSet) $
        IntSet.fromList [1, 2, 3]

    , compareClassifier $ Value (C_Prim C_Prim_ArrayM) $
        examplePrimArrayM

    , compareClassifier $ Value (C_Prim C_Vector_Storable) $
        SomeStorableVector $ unsafeCoerce $
          Vector.Storable.fromList ([1, 2] :: [Double])

    , compareClassifier $ Value (C_Prim C_Vector_StorableM) $
        exampleStorableVectorM

    , compareClassifier $ Value (C_Prim C_Vector_Primitive) $
        SomePrimitiveVector $ unsafeCoerce $
          Vector.Primitive.fromList ([1, 2] :: [Double])

    , compareClassifier $ Value (C_Prim C_Vector_PrimitiveM) $
        examplePrimitiveVectorM

      -- Compound

    , compareClassifier $ Value (C_Maybe ElemNothing) $
        Nothing
    , compareClassifier $ Value (C_Maybe (ElemJust (C_Prim C_Int))) $
        Just 3

    , compareClassifier $ Value (C_Either (ElemLeft (C_Prim C_Int))) $
        Left 3
    , compareClassifier $ Value (C_Either (ElemRight (C_Prim C_Bool))) $
        Right True

    , compareClassifier $ Value (C_List ElemNothing) $
        []
    , compareClassifier $ Value (C_List (ElemJust (C_Prim C_Int))) $
        [1, 2, 3]

    , compareClassifier $ Value (C_Tuple (Elems (Elem (C_Prim C_Int) :* Elem (C_Prim C_Char) :* Nil))) $
        WrappedTuple (4, 'a')
    , compareClassifier $ Value (C_Tuple (Elems (Elem (C_Prim C_Int) :* Elem (C_Prim C_Char) :* Elem (C_Prim C_Bool) :* Nil))) $
        WrappedTuple (4, 'a', True)

    , compareClassifier $ Value (C_Ratio (ElemJust (C_Prim C_Integer))) $
        1 % 2

    , compareClassifier $ Value (C_Set ElemNothing) $
        Set.empty
    , compareClassifier $ Value (C_Set (ElemJust (C_Prim C_Int))) $
        Set.fromList [1, 2, 3]

    , compareClassifier $ Value (C_Map ElemNothingPair) $
        Map.empty
    , compareClassifier $ Value (C_Map (ElemJustPair (C_Prim C_Int) (C_Prim C_Char))) $
        Map.fromList [(1, 'a'), (2, 'b')]

    , compareClassifier $ Value (C_IntMap ElemNothing) $
        IntMap.empty
    , compareClassifier $ Value (C_IntMap (ElemJust (C_Prim C_Char))) $
        IntMap.fromList [(1, 'a'), (2, 'b')]

    , compareClassifier $ Value (C_Sequence ElemNothing) $
        Seq.empty
    , compareClassifier $ Value (C_Sequence (ElemJust (C_Prim C_Int))) $
        Seq.fromList [1, 2, 3]

    , compareClassifier $ Value (C_Tree (ElemJust (C_Prim C_Int))) $
        Tree.Node 1 []

    , compareClassifier $ Value (C_HashSet (ElemJust (C_Prim C_Int))) $
        HashSet.fromList [1, 2, 3]

    , compareClassifier $ Value (C_HashMap ElemNothingPair) $
        HashMap.empty
    , compareClassifier $ Value (C_HashMap (ElemJustPair (C_Prim C_Int) (C_Prim C_Char))) $
        HashMap.fromList [(1, 'a'), (2, 'b')]

    , compareClassifier $ Value (C_HM_Array ElemNothing) $
        HashMap.Array.fromList 0 []
    , compareClassifier $ Value (C_HM_Array (ElemJust (C_Prim C_Int))) $
        HashMap.Array.fromList 2 [1, 2]

    , compareClassifier $ Value (C_Prim_Array ElemNothing) $
        Prim.Array.arrayFromList []
    , compareClassifier $ Value (C_Prim_Array (ElemJust (C_Prim C_Int))) $
        Prim.Array.arrayFromList [1, 2, 3]

    , compareClassifier $ Value (C_Vector_Boxed ElemNothing) $
        Vector.Boxed.empty
    , compareClassifier $ Value (C_Vector_Boxed (ElemJust (C_Prim C_Int))) $
        Vector.Boxed.fromList [1, 2, 3]

      -- User defined

    , compareClassifier $ Value (C_Other C_Simple) $
        SimpleA
    , compareClassifier $ Value (C_Other C_Simple) $
        SimpleB

    , compareClassifier $ Value (C_Other (C_NonRec ElemNothing))  $
        (NR1 1234)
    , compareClassifier $ Value (C_Other (C_NonRec (ElemJust (C_Prim C_Char)))) $
        (NR2 True 'a')

    , compareClassifier $ Value (C_Other (C_Rec ElemNothing)) $
        RNil
    , compareClassifier $ Value (C_Other (C_Rec (ElemJust (C_Prim C_Char)))) $
        (RCons 'a' RNil)

    , compareClassifier $ Value (C_Other C_Unlifted) $
        exampleContainsUnlifted
    ]
  where
    _checkAllCases :: ConcreteClassifier a -> ()
    _checkAllCases = \case
        C_Prim C_Bool     -> ()
        C_Prim C_Char     -> ()
        C_Prim C_Double   -> ()
        C_Prim C_Float    -> ()
        C_Prim C_Int      -> ()
        C_Prim C_Int8     -> ()
        C_Prim C_Int16    -> ()
        C_Prim C_Int32    -> ()
        C_Prim C_Int64    -> ()
        C_Prim C_Integer  -> ()
        C_Prim C_Ordering -> ()
        C_Prim C_Unit     -> ()
        C_Prim C_Word     -> ()
        C_Prim C_Word8    -> ()
        C_Prim C_Word16   -> ()
        C_Prim C_Word32   -> ()
        C_Prim C_Word64   -> ()

        -- String types

        C_Prim C_String      -> ()
        C_Prim C_BS_Strict   -> ()
        C_Prim C_BS_Lazy     -> ()
        C_Prim C_BS_Short    -> ()
        C_Prim C_Text_Strict -> ()
        C_Prim C_Text_Lazy   -> ()

        -- Aeson

        C_Prim C_Value -> ()

        -- Containers without type arguments

        C_Prim C_IntSet            -> ()
        C_Prim C_Prim_ArrayM       -> ()
        C_Prim C_Vector_Storable   -> ()
        C_Prim C_Vector_StorableM  -> ()
        C_Prim C_Vector_Primitive  -> ()
        C_Prim C_Vector_PrimitiveM -> ()

        -- Functions

        C_Prim C_Fun -> ()

        -- Reference cells

        C_Prim C_STRef -> ()
        C_Prim C_TVar  -> ()
        C_Prim C_MVar  -> ()

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
        C_Tuple{}        -> ()
        C_HashSet{}      -> ()
        C_HashMap{}      -> ()
        C_HM_Array{}     -> ()
        C_Prim_Array{}   -> ()
        C_Vector_Boxed{} -> ()

        -- User-defined

        C_Other (C_Simple{})   -> ()
        C_Other (C_NonRec{})   -> ()
        C_Other (C_Rec{})      -> ()
        C_Other (C_Unlifted{}) -> ()

-- | Test using arbitrary values
prop_arbitrary :: Some Value -> Property
prop_arbitrary (Some v) = compareClassifier v

-- | Compare given to inferred classifier
--
-- The tests in this module differ only in how the produce the 'Value's.
compareClassifier :: Value a -> Property
compareClassifier = \(Value cc x) ->
      counterexample ("Generated classifier: " ++ show cc)
    $ case runExcept $ classifyConcrete x of
        Left err  ->
            counterexample ("Failed to reclassify. Error: " ++ err)
          $ property False
        Right (Reclassified cc' _pf) ->
          case sameConcrete cc cc' of
            Nothing ->
                counterexample ("Inferred different classifier: " ++ show cc')
              $ property False
            Just Refl ->
              property True
