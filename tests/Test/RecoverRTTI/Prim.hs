{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Test.RecoverRTTI.Prim (
    -- * Equality
    canComparePrim
    -- * Arbitrary
  , Wrap(..)
  , primSatisfiesArbitrary
  , arbitraryPrimClassifier
  ) where

import Control.Monad (replicateM)
import Data.Int
import Data.IntSet (IntSet)
import Data.SOP (Compose)
import Data.SOP.Dict
import Data.String (fromString)
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as BS.Strict
import qualified Data.ByteString.Lazy  as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import qualified Data.Text             as Text.Strict
import qualified Data.Text.Lazy        as Text.Lazy
import qualified Data.Vector           as Vector.Boxed
import qualified Data.Vector.Primitive as Vector.Primitive
import qualified Data.Vector.Storable  as Vector.Storable

import Debug.RecoverRTTI

import Test.QuickCheck

import Test.RecoverRTTI.Classifier.Equality ()
import Test.RecoverRTTI.Globals

{-------------------------------------------------------------------------------
  Equality
-------------------------------------------------------------------------------}

canComparePrim :: PrimClassifier a -> Dict Eq a
canComparePrim = primSatisfies

{-------------------------------------------------------------------------------
  Arbitrary support for the primitive types
-------------------------------------------------------------------------------}

primSatisfiesArbitrary :: PrimClassifier a -> Dict (Compose Arbitrary Wrap) a
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
    , Some C_Prim_ArrayM
    , Some C_Vector_Storable
    , Some C_Vector_StorableM
    , Some C_Vector_Primitive
    , Some C_Vector_PrimitiveM
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
        C_Prim_ArrayM       -> ()
        C_Vector_Storable   -> ()
        C_Vector_StorableM  -> ()
        C_Vector_Primitive  -> ()
        C_Vector_PrimitiveM -> ()

{-------------------------------------------------------------------------------
  Arbitrary instances for specific types
  -------------------------------------------------------------------------------}

-- | 'Wrap' makes it possible to override an 'Arbitrary' instance if needed.
newtype Wrap a = Wrap { unwrap :: a }

deriving newtype instance Arbitrary (Wrap ())
deriving newtype instance Arbitrary (Wrap Bool)
deriving newtype instance Arbitrary (Wrap Char)
deriving newtype instance Arbitrary (Wrap Double)
deriving newtype instance Arbitrary (Wrap Float)
deriving newtype instance Arbitrary (Wrap Int)
deriving newtype instance Arbitrary (Wrap Int16)
deriving newtype instance Arbitrary (Wrap Int32)
deriving newtype instance Arbitrary (Wrap Int64)
deriving newtype instance Arbitrary (Wrap Int8)
deriving newtype instance Arbitrary (Wrap Integer)
deriving newtype instance Arbitrary (Wrap IntSet)
deriving newtype instance Arbitrary (Wrap Ordering)
deriving newtype instance Arbitrary (Wrap String)
deriving newtype instance Arbitrary (Wrap Word)
deriving newtype instance Arbitrary (Wrap Word16)
deriving newtype instance Arbitrary (Wrap Word32)
deriving newtype instance Arbitrary (Wrap Word64)
deriving newtype instance Arbitrary (Wrap Word8)

instance Arbitrary (Wrap BS.Strict.ByteString) where
  arbitrary = Wrap . BS.Strict.pack <$> arbitrary

instance Arbitrary (Wrap BS.Lazy.ByteString) where
  arbitrary = Wrap . BS.Lazy.pack <$> arbitrary

instance Arbitrary (Wrap BS.Short.ShortByteString) where
  arbitrary = Wrap . BS.Short.pack <$> arbitrary

instance Arbitrary (Wrap Text.Strict.Text) where
  arbitrary = Wrap . Text.Strict.pack <$> arbitrary

instance Arbitrary (Wrap Text.Lazy.Text) where
  arbitrary = Wrap . Text.Lazy.pack <$> arbitrary

-- | aeson >= 2.0.3.0 does define an 'Arbitrary' instance for 'Aeson.Value',
-- but it generates values that are too big, which cause the size sanity check
-- on the generator 'prop_showGenerated' to start to fail.
instance Arbitrary (Wrap Aeson.Value) where
  arbitrary = choose (0, 10) >>= fmap Wrap . go
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
                   <$> (fromString <$> fieldName)
                   <*> go (sz `div` n)
                 )
          ]

      -- We're not interested in testing crazy values
      fieldName :: Gen String
      fieldName = elements ["a", "b", "c"]

-- | Rather than trying to be clever here, we just generate a handful of
-- examples in different categories.
instance Arbitrary (Wrap SomeFun) where
  arbitrary = fmap Wrap $ elements [
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

instance Arbitrary (Wrap SomeStorableVector) where
  arbitrary = fmap Wrap $ elements [
        some $ Vector.Storable.fromList ([1, 2, 3] :: [Int])
      , some $ Vector.Storable.fromList ("abc"     :: String)
      ]
    where
      some :: Vector.Storable.Vector a -> SomeStorableVector
      some = unsafeCoerce

instance Arbitrary (Wrap SomePrimitiveVector) where
  arbitrary = fmap Wrap $ elements [
        some $ Vector.Primitive.fromList ([1, 2, 3] :: [Int])
      , some $ Vector.Primitive.fromList ("abc"     :: String)
      ]
    where
      some :: Vector.Primitive.Vector a -> SomePrimitiveVector
      some = unsafeCoerce

{-------------------------------------------------------------------------------
  For the mutable variables, we just use the one global example
-------------------------------------------------------------------------------}

instance Arbitrary (Wrap SomeSTRef) where
  arbitrary = return $ Wrap exampleSTRef

instance Arbitrary (Wrap SomeTVar) where
  arbitrary = return $ Wrap exampleTVar

instance Arbitrary (Wrap SomeMVar) where
  arbitrary = return $ Wrap exampleMVar

instance Arbitrary (Wrap SomePrimArrayM) where
  arbitrary = return $ Wrap examplePrimArrayM

instance Arbitrary (Wrap SomeStorableVectorM) where
  arbitrary = return $ Wrap exampleStorableVectorM

instance Arbitrary (Wrap SomePrimitiveVectorM) where
  arbitrary = return $ Wrap examplePrimitiveVectorM
