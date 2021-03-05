{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Test.RecoverRTTI.Arbitrary (
    ConcreteClassifier(..)
  , Value(..)
  , sameConcreteClassifier
    -- * Examples of user-defined types
  , NonRecursive(..)
  , Recursive(..)
    -- * Example values of reference cells
  , exampleIORef
  , exampleSTRef
  , exampleMVar
  , exampleTVar
  ) where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.Int
import Data.IORef (newIORef)
import Data.Kind
import Data.STRef (newSTRef)
import Data.Type.Equality
import Data.Void
import Data.Word
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Test.QuickCheck hiding (classify, NonEmpty)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Util

{-------------------------------------------------------------------------------
  Concrete classifier

  The difference between the " concrete " classifier and the 'Classifier' from
  the main library is that the former has explicit cases for user-defined types,
  and the latter doesn't (merely classifying them as 'UserDefined').

  In "Test.RecoverRRTI.Staged" we show that we can do staged inference,
  using 'classify' repeatedly to recover /all/ (concrete) type information
  from the type information returned by 'classify' (/if/ we have full
  information about which user-defined types we're interested in).
-------------------------------------------------------------------------------}

-- | Like 'Classifier', but with no guess-work and concrete types
data ConcreteClassifier (a :: Type) :: Type where
    -- Primitive types

    CC_Bool     :: ConcreteClassifier Bool
    CC_Char     :: ConcreteClassifier Char
    CC_Double   :: ConcreteClassifier Double
    CC_Float    :: ConcreteClassifier Float
    CC_Int      :: ConcreteClassifier Int
    CC_Int8     :: ConcreteClassifier Int8
    CC_Int16    :: ConcreteClassifier Int16
    CC_Int32    :: ConcreteClassifier Int32
    CC_Int64    :: ConcreteClassifier Int64
    CC_Ordering :: ConcreteClassifier Ordering
    CC_Unit     :: ConcreteClassifier ()
    CC_Word     :: ConcreteClassifier Word
    CC_Word8    :: ConcreteClassifier Word8
    CC_Word16   :: ConcreteClassifier Word16
    CC_Word32   :: ConcreteClassifier Word32
    CC_Word64   :: ConcreteClassifier Word64

    -- Compound

    CC_List :: MaybeEmpty ConcreteClassifier a -> ConcreteClassifier [a]

    -- Reference cells

    CC_STRef :: ConcreteClassifier SomeSTRef
    CC_TVar  :: ConcreteClassifier SomeTVar
    CC_MVar  :: ConcreteClassifier SomeMVar

    -- User-defined

    CC_User_NonRec :: MaybeEmpty ConcreteClassifier a -> ConcreteClassifier (NonRecursive a)
    CC_User_Rec    :: MaybeEmpty ConcreteClassifier a -> ConcreteClassifier (Recursive    a)

deriving instance Show (ConcreteClassifier a)
deriving instance Show (MaybeEmpty ConcreteClassifier a)

arbitraryClassifier :: forall r.
     (forall a. (Show a, Eq a) => ConcreteClassifier a -> Gen a -> Gen r)
  -> Gen r
arbitraryClassifier k = oneof [
      -- Primitive types

      k CC_Bool     arbitrary
    , k CC_Char     arbitrary
    , k CC_Double   arbitrary
    , k CC_Float    arbitrary
    , k CC_Int      arbitrary
    , k CC_Int16    arbitrary
    , k CC_Int8     arbitrary
    , k CC_Int32    arbitrary
    , k CC_Int64    arbitrary
    , k CC_Ordering arbitrary
    , k CC_Unit     arbitrary
    , k CC_Word     arbitrary
    , k CC_Word8    arbitrary
    , k CC_Word16   arbitrary
    , k CC_Word32   arbitrary
    , k CC_Word64   arbitrary

      -- Compound

    , arbitraryClassifier $
        genMaybeEmpty
          CC_List
          (return [])
          (\gen -> (:) <$> gen <*> listOf gen)

      -- Reference cells

    , k CC_STRef (pure exampleSTRef)
    , k CC_STRef (pure exampleIORef)
    , k CC_MVar  (pure exampleMVar)
    , k CC_TVar  (pure exampleTVar)

      -- User-defined

    , arbitraryClassifier $
        genMaybeEmpty
          CC_User_NonRec
          (NR1 <$> arbitrary)
          (\gen -> NR2 <$> gen <*> arbitrary)
    , arbitraryClassifier $
        genMaybeEmpty
          CC_User_Rec
          (return RNil)
          (\gen -> RCons <$> gen <*> (recursiveFromList <$> listOf gen))
    ]
  where
    genMaybeEmpty ::
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         , Show a
         , Eq   a
         )
      => (forall x. MaybeEmpty ConcreteClassifier x -> ConcreteClassifier (f x))
      -> Gen (f Void)
      -> (forall x. Gen x -> Gen (f x))
      -> ConcreteClassifier a
      -> Gen a
      -> Gen r
    genMaybeEmpty cc genEmpty genNonEmpty c genA = oneof [
          k (cc Empty)         genEmpty
        , k (cc (NonEmpty c)) (genNonEmpty genA)
        ]

    -- ConcreteClassifier a -> Gen a -> Gen r

    -- We check that we cover all cases of 'Classifier' rather than
    -- 'ConcreteClassifier': it is important that we generate test cases for
    -- everything we classify in the main library.
    _checkAllCases :: Classifier a -> ()
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
         C_Ordering -> ()
         C_Unit     -> ()
         C_Word     -> ()
         C_Word8    -> ()
         C_Word16   -> ()
         C_Word32   -> ()
         C_Word64   -> ()

         -- Compound

         C_List{} -> ()

         -- Reference cells

         C_STRef -> ()
         C_TVar  -> ()
         C_MVar  -> ()

         -- User-defined

         C_Custom{} -> ()

         -- We don't generate values that we cannot classify

         C_Unknown -> ()

{-------------------------------------------------------------------------------
  Equality
-------------------------------------------------------------------------------}

-- | Check that two classifiers are the same
--
-- If they are the same, additionally return a proof that that means the
-- /types/ they classify must be equal (note that equality on the classifiers
-- is strictly stronger than equality on the types: for example, non-empty
-- and empty lists have different classifiers, but classify the same type).
sameConcreteClassifier ::
     ConcreteClassifier a
  -> ConcreteClassifier b
  -> Maybe (a :~: b)
sameConcreteClassifier = go
  where
    go :: ConcreteClassifier a -> ConcreteClassifier b -> Maybe (a :~: b)
    go CC_Bool     CC_Bool     = Just Refl
    go CC_Char     CC_Char     = Just Refl
    go CC_Double   CC_Double   = Just Refl
    go CC_Float    CC_Float    = Just Refl
    go CC_Int      CC_Int      = Just Refl
    go CC_Int8     CC_Int8     = Just Refl
    go CC_Int16    CC_Int16    = Just Refl
    go CC_Int32    CC_Int32    = Just Refl
    go CC_Int64    CC_Int64    = Just Refl
    go CC_Ordering CC_Ordering = Just Refl
    go CC_Unit     CC_Unit     = Just Refl
    go CC_Word     CC_Word     = Just Refl
    go CC_Word8    CC_Word8    = Just Refl
    go CC_Word16   CC_Word16   = Just Refl
    go CC_Word32   CC_Word32   = Just Refl
    go CC_Word64   CC_Word64   = Just Refl

    -- Compound

    go (CC_List c) (CC_List c') = goF c c'

    -- Reference cells

    go CC_STRef CC_STRef = Just Refl
    go CC_TVar  CC_TVar  = Just Refl
    go CC_MVar  CC_MVar  = Just Refl

    -- User-defined

    go (CC_User_NonRec c) (CC_User_NonRec c') = goF c c'
    go (CC_User_Rec    c) (CC_User_Rec    c') = goF c c'

    -- Otherwise, not equal

    go _ _ = Nothing

    goF :: MaybeEmpty ConcreteClassifier a
        -> MaybeEmpty ConcreteClassifier b
        -> Maybe (f a :~: f b)
    goF Empty        Empty         = Just Refl
    goF (NonEmpty c) (NonEmpty c') = (\Refl -> Refl) <$> go c c'
    goF _            _             = Nothing

    -- Make sure we get a warning if we add another constructor
    _checkAllCases :: ConcreteClassifier a -> ()
    _checkAllCases = \case
        -- Primitive types

        CC_Bool     -> ()
        CC_Char     -> ()
        CC_Double   -> ()
        CC_Float    -> ()
        CC_Int      -> ()
        CC_Int8     -> ()
        CC_Int16    -> ()
        CC_Int32    -> ()
        CC_Int64    -> ()
        CC_Ordering -> ()
        CC_Unit     -> ()
        CC_Word     -> ()
        CC_Word8    -> ()
        CC_Word16   -> ()
        CC_Word32   -> ()
        CC_Word64   -> ()

        -- Compound

        CC_List{} -> ()

        -- Reference cells

        CC_STRef -> ()
        CC_TVar  -> ()
        CC_MVar  -> ()

        -- User-defined

        CC_User_NonRec{} -> ()
        CC_User_Rec{}    -> ()


{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Like 'Classified', but using 'ConcreteClassifier'
--
-- For convenience, we also include some constraints here, even though they
-- are in fact derivable from the classifier
data Value a where
   Value :: (Show a, Eq a) => ConcreteClassifier a -> a -> Value a

deriving instance Show (Value a)
deriving instance Show (Some Value)

instance Arbitrary (Some Value) where
  arbitrary = arbitraryClassifier $ \cc gen -> Exists . Value cc <$> gen

{-------------------------------------------------------------------------------
  User-defined datatypes
-------------------------------------------------------------------------------}

-- | Example of a non-recursive user-defined type
data NonRecursive a = NR1 Int | NR2 a Bool
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Example of a recursive user-defined type
data Recursive a = RNil | RCons a (Recursive a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

recursiveFromList :: [a] -> Recursive a
recursiveFromList = foldr RCons RNil

{-------------------------------------------------------------------------------
  Some global variables, which we use only as input to the tests
-------------------------------------------------------------------------------}

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
