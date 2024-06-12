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
import Data.STRef (newSTRef)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

#if MIN_VERSION_base(4,17,0)
import qualified GHC.IsList as IsList
#else
import qualified GHC.Exts as IsList (fromList)
#endif

import qualified Data.Primitive.Array     as Prim.Array
import qualified Data.Primitive.ByteArray as Prim.ByteArray
import qualified Data.Vector.Primitive    as Vector.Primitive
import qualified Data.Vector.Storable     as Vector.Storable

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
