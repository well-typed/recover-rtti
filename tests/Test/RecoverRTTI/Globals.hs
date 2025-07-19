{-# LANGUAGE CPP #-}

-- | Global mutable variables
--
-- The tests are entirely pure, but occassionally needs examples of these
-- mutable structures. Having a single global example available is convenient.
module Test.RecoverRTTI.Globals (
    exampleIORef
  , exampleSTRef
  , exampleMVar
  , exampleTVar
  , examplePrimArrayM
  , exampleStorableVectorM
  , examplePrimitiveVectorM
  , exampleMutableByteArray
  ) where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.IORef (newIORef)
import Data.Primitive.Array qualified as Prim.Array
import Data.Primitive.ByteArray qualified as Prim.ByteArray
import Data.STRef (newSTRef)
import Data.Vector.Primitive qualified as Vector.Primitive
import Data.Vector.Storable qualified as Vector.Storable
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

#if MIN_VERSION_base(4,17,0)
import GHC.IsList qualified as IsList
#else
import GHC.Exts qualified as IsList (fromList)
#endif

import Debug.RecoverRTTI

exampleIORef :: SomeSTRef
{-# NOINLINE exampleIORef #-}
exampleIORef = unsafePerformIO $
    -- IORef is indistinguishable from STRef on the heap
    unsafeCoerce <$> newIORef (unsafeCoerce ())

exampleSTRef :: SomeSTRef
exampleSTRef = unsafePerformIO $ unsafeSTToIO $
    unsafeCoerce <$> newSTRef (unsafeCoerce ())

exampleMVar :: SomeMVar
{-# NOINLINE exampleMVar #-}
exampleMVar = unsafePerformIO $
    SomeMVar <$> newEmptyMVar

exampleTVar :: SomeTVar
{-# NOINLINE exampleTVar #-}
exampleTVar = unsafePerformIO $
    SomeTVar <$> newTVarIO (unsafeCoerce ())

examplePrimArrayM :: SomePrimArrayM
{-# NOINLINE examplePrimArrayM #-}
examplePrimArrayM = unsafePerformIO $
    unsafeCoerce <$> Prim.Array.newArray 0 (error "no elements")

exampleStorableVectorM :: SomeStorableVectorM
{-# NOINLINE exampleStorableVectorM #-}
exampleStorableVectorM = unsafePerformIO $
    unsafeCoerce <$> Vector.Storable.thaw (Vector.Storable.fromList "abc")

examplePrimitiveVectorM :: SomePrimitiveVectorM
{-# NOINLINE examplePrimitiveVectorM #-}
examplePrimitiveVectorM = unsafePerformIO $
    unsafeCoerce <$> Vector.Primitive.thaw (Vector.Primitive.fromList "abc")

exampleMutableByteArray :: SomeMutableByteArray
{-# NOINLINE exampleMutableByteArray #-}
exampleMutableByteArray = unsafePerformIO $
    SomeMutableByteArray <$> Prim.ByteArray.thawByteArray (IsList.fromList [0, 1, 2]) 0 3
