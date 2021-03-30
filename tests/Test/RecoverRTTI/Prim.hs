{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.RecoverRTTI.Prim (
    -- * Equality
    canComparePrim
    -- * Arbitrary
  , primSatisfiesArbitrary
  , arbitraryPrimClassifier
  ) where

import Control.Monad
import Data.SOP.Dict
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as BS.Strict
import qualified Data.ByteString.Lazy  as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import qualified Data.Text             as Text.Strict
import qualified Data.Text.Lazy        as Text.Lazy
import qualified Data.Vector           as Vector.Boxed
import qualified Data.Vector.Storable  as Vector.Storable
import qualified Data.Vector.Primitive as Vector.Primitive

import Debug.RecoverRTTI

import Test.QuickCheck

import Test.RecoverRTTI.Globals

{-------------------------------------------------------------------------------
  Equality
-------------------------------------------------------------------------------}

canComparePrim :: PrimClassifier a -> Dict Eq a
canComparePrim = primSatisfies

{-------------------------------------------------------------------------------
  Arbitrary support for the primitive types
-------------------------------------------------------------------------------}

primSatisfiesArbitrary :: PrimClassifier a -> Dict Arbitrary a
primSatisfiesArbitrary = primSatisfies

arbitraryPrimClassifier :: Gen (Some PrimClassifier)
arbitraryPrimClassifier = elements [
    -- Primitive types

      Some C_Bool
    , Some C_Char
    , Some C_Double
    , Some C_Float
    , Some C_Int
    , Some C_Int16
    , Some C_Int8
    , Some C_Int32
    , Some C_Int64
    , Some C_Integer
    , Some C_Ordering
    , Some C_Unit
    , Some C_Word
    , Some C_Word8
    , Some C_Word16
    , Some C_Word32
    , Some C_Word64

    -- String types

    , Some C_String
    , Some C_BS_Strict
    , Some C_BS_Lazy
    , Some C_BS_Short
    , Some C_Text_Strict
    , Some C_Text_Lazy

    -- Aeson

    , Some C_Value

    -- Reference cells

    , Some C_STRef
    , Some C_TVar
    , Some C_MVar

    -- Functions

    , Some C_Fun

    -- Containers with no type arguments

    , Some C_IntSet
    , Some C_Prim_MArray
    , Some C_Vector_Storable
    , Some C_Vector_MStorable
    , Some C_Vector_Primitive
    , Some C_Vector_MPrimitive
    ]
  where
    _checkAllCases :: PrimClassifier a -> ()
    _checkAllCases = \case
        -- Primitive types

        C_Bool     -> ()
        C_Char     -> ()
        C_Double   -> ()
        C_Float    -> ()
        C_Int      -> ()
        C_Int16    -> ()
        C_Int8     -> ()
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

        -- Functions

        C_Fun -> ()

        -- Containers with no type arguments

        C_IntSet            -> ()
        C_Prim_MArray       -> ()
        C_Vector_Storable   -> ()
        C_Vector_MStorable  -> ()
        C_Vector_Primitive  -> ()
        C_Vector_MPrimitive -> ()

{-------------------------------------------------------------------------------
  Orphan instances
-------------------------------------------------------------------------------}

instance Arbitrary BS.Strict.ByteString where
  arbitrary = BS.Strict.pack <$> arbitrary

instance Arbitrary BS.Lazy.ByteString where
  arbitrary = BS.Lazy.pack <$> arbitrary

instance Arbitrary BS.Short.ShortByteString where
  arbitrary = BS.Short.pack <$> arbitrary

instance Arbitrary Text.Strict.Text where
  arbitrary = Text.Strict.pack <$> arbitrary

instance Arbitrary Text.Lazy.Text where
  arbitrary = Text.Lazy.pack <$> arbitrary

instance Arbitrary Aeson.Value where
  arbitrary = choose (0, 10) >>= go
    where
      go :: Int -> Gen Aeson.Value
      go 0  = oneof nonRecursive
      go sz = oneof (nonRecursive ++ recursive sz)

      nonRecursive :: [Gen Aeson.Value]
      nonRecursive = [
            Aeson.String . Text.Strict.pack <$> arbitrary
          , Aeson.Number . fromInteger <$> arbitrary
          , Aeson.Bool <$> arbitrary
          , return Aeson.Null
          ]

      recursive :: Int -> [Gen Aeson.Value]
      recursive sz = [
            do n <- choose (0, 5)
               Aeson.Array . Vector.Boxed.fromList <$> replicateM n (go (sz `div` n))
          , do n <- choose (0, 5)
               Aeson.object <$> replicateM n (
                       (Aeson..=)
                   <$> fieldName
                   <*> go (sz `div` n)
                 )
          ]

      -- We're not interested in testing crazy values
      fieldName :: Gen Text.Strict.Text
      fieldName = elements ["a", "b", "c"]

-- | Rather than trying to be clever here, we just generate a handful of
-- examples in different categories.
instance Arbitrary SomeFun where
  arbitrary = elements [
        -- Parametrically polymorphic function
        fun (id    :: Int -> Int)
      , fun (const :: Int -> Bool -> Int)
        -- Ad-hoc polymorphic function
      , fun (negate :: Int -> Int)
      , fun ((+)    :: Int -> Int -> Int)
        -- Partial application
      , fun (const 1 :: Bool -> Int)
      , fun ((+)   1 :: Int -> Int)
      ]
    where
      fun :: (a -> b) -> SomeFun
      fun = unsafeCoerce

instance Arbitrary SomeStorableVector where
  arbitrary = elements [
        some $ Vector.Storable.fromList ([1, 2, 3] :: [Int])
      , some $ Vector.Storable.fromList ("abc"     :: String)
      ]
    where
      some :: Vector.Storable.Vector a -> SomeStorableVector
      some = unsafeCoerce

instance Arbitrary SomePrimitiveVector where
  arbitrary = elements [
        some $ Vector.Primitive.fromList ([1, 2, 3] :: [Int])
      , some $ Vector.Primitive.fromList ("abc"     :: String)
      ]
    where
      some :: Vector.Primitive.Vector a -> SomePrimitiveVector
      some = unsafeCoerce

{-------------------------------------------------------------------------------
  For the mutable variables, we just use the one global example
-------------------------------------------------------------------------------}

instance Arbitrary SomeSTRef where
  arbitrary = return exampleSTRef

instance Arbitrary SomeTVar where
  arbitrary = return exampleTVar

instance Arbitrary SomeMVar where
  arbitrary = return exampleMVar

instance Arbitrary SomePrimMutableArray where
  arbitrary = return examplePrimMArray

instance Arbitrary SomeStorableMVector where
  arbitrary = return exampleStorableMVector

instance Arbitrary SomePrimitiveMVector where
  arbitrary = return examplePrimitiveMVector

{-------------------------------------------------------------------------------
  Orphan equality instances
-------------------------------------------------------------------------------}

-- | Degenerate 'Eq' instance for functions that always says 'True'
--
-- When we compare values up to the coercion returned by 'reclassify', we need
-- an 'Eq' instance. We can't compare functions in any meaningful way though,
-- and so we just return 'True' here no matter what.
--
-- This is an orphan defined in the test suite only, so that users of the
-- library don't have acccess to this (misleading) instance.
instance Eq SomeFun where
  _ == _ = True

instance Eq SomeStorableVector where
  _ == _ = True

instance Eq SomeStorableMVector where
  _ == _ = True

instance Eq SomePrimitiveVector where
  _ == _ = True

instance Eq SomePrimitiveMVector where
  _ == _ = True
