{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Verify we infer the right classifier
module Test.RecoverRTTI.Classify (tests) where

import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.HashMap.Internal.Array qualified as HashMap.Array
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map
import Data.Ratio
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.SOP
import Data.Tree qualified as Tree
import Data.Vector qualified as Vector.Boxed
import Data.Vector.Primitive qualified as Vector.Primitive
import Data.Vector.Storable qualified as Vector.Storable
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Unsafe.Coerce (unsafeCoerce)

#if MIN_VERSION_base(4,17,0)
import GHC.IsList qualified as IsList
#else
import GHC.Exts qualified as IsList (fromList)
#endif

import Debug.RecoverRTTI

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.ConcreteClassifier.Value
import Test.RecoverRTTI.Globals
import Test.RecoverRTTI.Staged
import Test.RecoverRTTI.UserDefined
import Test.RecoverRTTI.ConcreteClassifier.Compatibility

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
prop_constants = QC.withMaxSuccess 1 $ QC.conjoin [
      -- Primitive types

      compareClassifier $ Value (CC_Prim C_Bool)     True
    , compareClassifier $ Value (CC_Prim C_Bool)     False
    , compareClassifier $ Value (CC_Prim C_Char)     'a'
    , compareClassifier $ Value (CC_Prim C_Double)   1.25
    , compareClassifier $ Value (CC_Prim C_Float)    1.25
    , compareClassifier $ Value (CC_Prim C_Int)      1234
    , compareClassifier $ Value (CC_Prim C_Int)      (-1234)
    , compareClassifier $ Value (CC_Prim C_Int8)     123
    , compareClassifier $ Value (CC_Prim C_Int16)    1234
    , compareClassifier $ Value (CC_Prim C_Int32)    1234
    , compareClassifier $ Value (CC_Prim C_Int64)    1234
    , compareClassifier $ Value (CC_Prim C_Integer)  1234
    , compareClassifier $ Value (CC_Prim C_Integer)  (succ (fromIntegral (maxBound :: Int)))
    , compareClassifier $ Value (CC_Prim C_Integer)  (pred (fromIntegral (minBound :: Int)))
    , compareClassifier $ Value (CC_Prim C_Ordering) LT
    , compareClassifier $ Value (CC_Prim C_Ordering) GT
    , compareClassifier $ Value (CC_Prim C_Ordering) EQ
    , compareClassifier $ Value (CC_Prim C_Unit)     ()
    , compareClassifier $ Value (CC_Prim C_Word)     1234
    , compareClassifier $ Value (CC_Prim C_Word8)    123
    , compareClassifier $ Value (CC_Prim C_Word16)   134
    , compareClassifier $ Value (CC_Prim C_Word32)   1234
    , compareClassifier $ Value (CC_Prim C_Word64)   1234

      -- String types
      --
      -- We skip the empty string, because we infer that as @C_List Empty@

    , compareClassifier $ Value (CC_Prim C_BS_Strict)   ""
    , compareClassifier $ Value (CC_Prim C_BS_Strict)   "abcdefg"
    , compareClassifier $ Value (CC_Prim C_BS_Lazy)     ""
    , compareClassifier $ Value (CC_Prim C_BS_Lazy)     "abcdefg"
    , compareClassifier $ Value (CC_Prim C_Text_Strict) ""
    , compareClassifier $ Value (CC_Prim C_Text_Strict) "abcdefg"
    , compareClassifier $ Value (CC_Prim C_Text_Lazy)   ""
    , compareClassifier $ Value (CC_Prim C_Text_Lazy)   "abcdefg"

#if !MIN_VERSION_bytestring(0,12,0)
    , compareClassifier $ Value (CC_Prim C_BS_Short)    ""
    , compareClassifier $ Value (CC_Prim C_BS_Short)    "abcdefg"
#endif

      -- Aeson

    , compareClassifier $ Value (CC_Prim C_Value) (Aeson.object [("x" Aeson..= True)])

      -- Reference cells

    , compareClassifier $ Value (CC_Prim C_STRef) exampleIORef
    , compareClassifier $ Value (CC_Prim C_STRef) exampleSTRef
    , compareClassifier $ Value (CC_Prim C_MVar)  exampleMVar
    , compareClassifier $ Value (CC_Prim C_TVar)  exampleTVar

      -- Functions

    , compareClassifier $ Value (CC_Prim C_Fun) (SomeFun id)

      -- Containers without type arguments

    , compareClassifier $ Value (CC_Prim C_IntSet) $
        IntSet.empty
    , compareClassifier $ Value (CC_Prim C_IntSet) $
        IntSet.fromList [1, 2, 3]

    , compareClassifier $ Value (CC_Prim C_Prim_ArrayM) $
        examplePrimArrayM

    , compareClassifier $ Value (CC_Prim C_Vector_Storable) $
        SomeStorableVector $ unsafeCoerce $
          Vector.Storable.fromList ([1, 2] :: [Double])

    , compareClassifier $ Value (CC_Prim C_Vector_StorableM) $
        exampleStorableVectorM

    , compareClassifier $ Value (CC_Prim C_Vector_Primitive) $
        SomePrimitiveVector $ unsafeCoerce $
          Vector.Primitive.fromList ([1, 2] :: [Double])

    , compareClassifier $ Value (CC_Prim C_Vector_PrimitiveM) $
        examplePrimitiveVectorM

    , compareClassifier $ Value (CC_Prim C_ByteArray) $
        IsList.fromList [0, 1, 2]
    , compareClassifier $ Value (CC_Prim C_MutableByteArray) $
        exampleMutableByteArray

      -- Compound

    , compareClassifier $ Value (CC_Maybe CC_Void) $
        Nothing
    , compareClassifier $ Value (CC_Maybe (CC_Prim C_Int)) $
        Just 3

    , compareClassifier $ Value (CC_Either (CC_Prim C_Int) CC_Void) $
        Left 3
    , compareClassifier $ Value (CC_Either CC_Void (CC_Prim C_Bool)) $
        Right True

    , compareClassifier $ Value (CC_List CC_Void) $
        []
    , compareClassifier $ Value (CC_List (CC_Prim C_Int)) $
        [1, 2, 3]

    , compareClassifier $ Value (CC_Tuple (Concretes (CC_Prim C_Int :* CC_Prim C_Char :* Nil))) $
        WrappedTuple (4, 'a')
    , compareClassifier $ Value (CC_Tuple (Concretes (CC_Prim C_Int :* CC_Prim C_Char :* CC_Prim C_Bool :* Nil))) $
        WrappedTuple (4, 'a', True)

    , compareClassifier $ Value (CC_Ratio (CC_Prim C_Integer)) $
        1 % 2

    , compareClassifier $ Value (CC_Set CC_Void) $
        Set.empty
    , compareClassifier $ Value (CC_Set (CC_Prim C_Int)) $
        Set.fromList [1, 2, 3]

    , compareClassifier $ Value (CC_Map CC_Void CC_Void) $
        Map.empty
    , compareClassifier $ Value (CC_Map (CC_Prim C_Int) (CC_Prim C_Char)) $
        Map.fromList [(1, 'a'), (2, 'b')]

    , compareClassifier $ Value (CC_IntMap CC_Void) $
        IntMap.empty
    , compareClassifier $ Value (CC_IntMap (CC_Prim C_Char)) $
        IntMap.fromList [(1, 'a'), (2, 'b')]

    , compareClassifier $ Value (CC_Sequence CC_Void) $
        Seq.empty
    , compareClassifier $ Value (CC_Sequence (CC_Prim C_Int)) $
        Seq.fromList [1, 2, 3]

    , compareClassifier $ Value (CC_Tree (CC_Prim C_Int)) $
        Tree.Node 1 []

    , compareClassifier $ Value (CC_HashSet (CC_Prim C_Int)) $
        HashSet.fromList [1, 2, 3]

    , compareClassifier $ Value (CC_HashMap CC_Void CC_Void) $
        HashMap.empty
    , compareClassifier $ Value (CC_HashMap (CC_Prim C_Int) (CC_Prim C_Char)) $
        HashMap.fromList [(1, 'a'), (2, 'b')]

    , compareClassifier $ Value (CC_HM_Array CC_Void) $
        HashMap.Array.fromList 0 []
    , compareClassifier $ Value (CC_HM_Array (CC_Prim C_Int)) $
        HashMap.Array.fromList 2 [1, 2]

    , compareClassifier $ Value (CC_Prim_Array CC_Void) $
        IsList.fromList []
    , compareClassifier $ Value (CC_Prim_Array (CC_Prim C_Int)) $
        IsList.fromList [1, 2, 3]

    , compareClassifier $ Value (CC_Vector_Boxed CC_Void) $
        Vector.Boxed.empty
    , compareClassifier $ Value (CC_Vector_Boxed (CC_Prim C_Int)) $
        Vector.Boxed.fromList [1, 2, 3]

      -- User defined

    , compareClassifier $ Value (CC_Other CC_Simple) $
        SimpleA
    , compareClassifier $ Value (CC_Other CC_Simple) $
        SimpleB

    , compareClassifier $ Value (CC_Other (CC_NonRec CC_Void))  $
        (NR1 1234)
    , compareClassifier $ Value (CC_Other (CC_NonRec (CC_Prim C_Char))) $
        (NR2 True 'a')

    , compareClassifier $ Value (CC_Other (CC_Rec CC_Void)) $
        RNil
    , compareClassifier $ Value (CC_Other (CC_Rec (CC_Prim C_Char))) $
        (RCons 'a' RNil)

    , compareClassifier $ Value (CC_Other CC_Unlifted) $
        exampleContainsUnlifted
    ]
  where
    _checkAllCases :: Concrete a -> ()
    _checkAllCases = \case
        -- Void (used only as an argument to containers)
        CC_Void            -> ()

        -- Primitive types
        CC_Prim C_Bool     -> ()
        CC_Prim C_Char     -> ()
        CC_Prim C_Double   -> ()
        CC_Prim C_Float    -> ()
        CC_Prim C_Int      -> ()
        CC_Prim C_Int8     -> ()
        CC_Prim C_Int16    -> ()
        CC_Prim C_Int32    -> ()
        CC_Prim C_Int64    -> ()
        CC_Prim C_Integer  -> ()
        CC_Prim C_Ordering -> ()
        CC_Prim C_Unit     -> ()
        CC_Prim C_Word     -> ()
        CC_Prim C_Word8    -> ()
        CC_Prim C_Word16   -> ()
        CC_Prim C_Word32   -> ()
        CC_Prim C_Word64   -> ()

        -- String types
        CC_Prim C_BS_Strict   -> ()
        CC_Prim C_BS_Lazy     -> ()
        CC_Prim C_Text_Strict -> ()
        CC_Prim C_Text_Lazy   -> ()

#if !MIN_VERSION_bytestring(0,12,0)
        CC_Prim C_BS_Short    -> ()
#endif

        -- Aeson
        CC_Prim C_Value -> ()

        -- Containers without type arguments
        CC_Prim C_IntSet            -> ()
        CC_Prim C_Prim_ArrayM       -> ()
        CC_Prim C_Vector_Storable   -> ()
        CC_Prim C_Vector_StorableM  -> ()
        CC_Prim C_Vector_Primitive  -> ()
        CC_Prim C_Vector_PrimitiveM -> ()
        CC_Prim C_ByteArray         -> ()
        CC_Prim C_MutableByteArray  -> ()

        -- Functions
        CC_Prim C_Fun -> ()

        -- Reference cells
        CC_Prim C_STRef -> ()
        CC_Prim C_TVar  -> ()
        CC_Prim C_MVar  -> ()

        -- Compound
        CC_Either{}       -> ()
        CC_HashMap{}      -> ()
        CC_HashSet{}      -> ()
        CC_HM_Array{}     -> ()
        CC_IntMap{}       -> ()
        CC_List{}         -> ()
        CC_Map{}          -> ()
        CC_Maybe{}        -> ()
        CC_Prim_Array{}   -> ()
        CC_Ratio{}        -> ()
        CC_Sequence{}     -> ()
        CC_Set{}          -> ()
        CC_Tree{}         -> ()
        CC_Tuple{}        -> ()
        CC_Vector_Boxed{} -> ()

        -- User-defined
        CC_Other (CC_Simple{})   -> ()
        CC_Other (CC_NonRec{})   -> ()
        CC_Other (CC_Rec{})      -> ()
        CC_Other (CC_Unlifted{}) -> ()

-- | Test using arbitrary values
prop_arbitrary :: Some Value -> Property
prop_arbitrary (Some v) = compareClassifier v

-- | Compare given to inferred classifier
--
-- The tests in this module differ only in how the produce the 'Value's.
compareClassifier :: Value a -> Property
compareClassifier = \(Value cc x) ->
      QC.counterexample ("Generated classifier: " ++ show cc)
    $ case runExcept $ classifyConcrete x of
        Left err  ->
            QC.counterexample ("Failed to reclassify. Error: " ++ err)
          $ QC.property False
        Right (Reclassified cc' _pf) ->
          case compatibleClassifier cc cc' of
            Nothing ->
                QC.counterexample ("Inferred different classifier: " ++ show cc')
              $ QC.property False
            Just _ ->
              QC.property True

