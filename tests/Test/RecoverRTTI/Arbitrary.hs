{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.RecoverRTTI.Arbitrary (
    ClassifiedGen(..)
  , arbitraryClassifiedGen
    -- * Example values of reference cells
  , exampleIORef
  , exampleSTRef
  , exampleMVar
  , exampleTVar
  ) where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.IORef (newIORef)
import Data.Maybe (catMaybes)
import Data.SOP
import Data.SOP.Dict
import Data.STRef (newSTRef)
import Data.Void
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.ByteString       as BS.Strict
import qualified Data.ByteString.Lazy  as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import qualified Data.Text             as Text.Strict
import qualified Data.Text.Lazy        as Text.Lazy

import Test.QuickCheck hiding (classify, NonEmpty)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Util.TypeLevel

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.Orphans ()
import Test.RecoverRTTI.UserDefined

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

newtype SizedGen a = SizedGen (Int -> Gen a)
  deriving (Functor)

runSized :: Int -> SizedGen a -> Gen a
runSized n (SizedGen gen) = gen n

ignoreSize :: Gen a -> SizedGen a
ignoreSize gen = SizedGen $ \_sz -> gen

{-------------------------------------------------------------------------------
  Arbitrary instance
-------------------------------------------------------------------------------}

-- | Quickcheck generator along with a classifier
data ClassifiedGen a where
  ClassifiedGen ::
       (Show a, Eq a)
    => { -- | The classifier for the generator
         genClassifier :: ConcreteClassifier a

         -- | The classified generator itself
         --
         -- The size argument determines the maximum size of the /value/
         -- (as opposed to the maximum size of the /type/)
       , classifiedGen :: SizedGen a
       }
    -> ClassifiedGen a

canShowClassifiedGen :: ClassifiedGen a -> Dict Show a
canShowClassifiedGen ClassifiedGen{} = Dict

canEqClassifiedGen :: ClassifiedGen a -> Dict Eq a
canEqClassifiedGen ClassifiedGen{} = Dict

defaultClassifiedGen ::
     (Arbitrary a, Show a, Eq a)
  => ConcreteClassifier a
  -> ClassifiedGen a
defaultClassifiedGen cc = ClassifiedGen cc $ ignoreSize arbitrary

-- | Generated arbitrary classifier along with a generator for that value
--
-- NOTE: The @sz@ parameter limits the size of the /type tree/ (i.e., the number
-- of recursive calls to arbitraryClassifiedGen), /not/ the size of the
-- generated /values/.
arbitraryClassifiedGen :: Int -> Gen (Some ClassifiedGen)
arbitraryClassifiedGen typSz
  | typSz <  0 = error "arbitraryClassifiedGen: uhoh.. bug"
  | typSz == 0 = elements leaves
  | otherwise  = oneof (elements leaves : catMaybes compound)
  where
    -- Leaves of the tree (values with no recursion)
    --
    -- Since there are the leaves, we don't need to check the size
    leaves :: [Some ClassifiedGen]
    leaves = concat [
          -- Primitive types
          [ Some $ defaultClassifiedGen CC_Bool
          , Some $ defaultClassifiedGen CC_Char
          , Some $ defaultClassifiedGen CC_Double
          , Some $ defaultClassifiedGen CC_Float
          , Some $ defaultClassifiedGen CC_Int
          , Some $ defaultClassifiedGen CC_Int16
          , Some $ defaultClassifiedGen CC_Int8
          , Some $ defaultClassifiedGen CC_Int32
          , Some $ defaultClassifiedGen CC_Int64
          , Some $ defaultClassifiedGen CC_Ordering
          , Some $ defaultClassifiedGen CC_Unit
          , Some $ defaultClassifiedGen CC_Word
          , Some $ defaultClassifiedGen CC_Word8
          , Some $ defaultClassifiedGen CC_Word16
          , Some $ defaultClassifiedGen CC_Word32
          , Some $ defaultClassifiedGen CC_Word64
         ]

          -- Strings
          --
          -- Avoid generating the empty string (recognized as @[Void]@)
        , let mapList :: Arbitrary a => Int -> ([a] -> b) -> SizedGen b
              mapList minSize f = SizedGen $ \valSz -> do
                  n <- choose (minSize, max minSize valSz) -- maybe valSz == 0
                  f <$> vector n
          in [
             Some $ ClassifiedGen CC_String      (mapList 1 id)
           , Some $ ClassifiedGen CC_BS_Strict   (mapList 0 BS.Strict.pack)
           , Some $ ClassifiedGen CC_BS_Lazy     (mapList 0 BS.Lazy.pack)
           , Some $ ClassifiedGen CC_BS_Short    (mapList 0 BS.Short.pack)
           , Some $ ClassifiedGen CC_Text_Strict (mapList 0 Text.Strict.pack)
           , Some $ ClassifiedGen CC_Text_Lazy   (mapList 0 Text.Lazy.pack)
          ]

          -- Reference cells
        , [ Some $ ClassifiedGen CC_STRef (ignoreSize $ pure exampleSTRef)
          , Some $ ClassifiedGen CC_STRef (ignoreSize $ pure exampleIORef)
          , Some $ ClassifiedGen CC_MVar  (ignoreSize $ pure exampleMVar)
          , Some $ ClassifiedGen CC_TVar  (ignoreSize $ pure exampleTVar)
          ]

          -- Functions
          --
          -- For functions we don't currently try to be clever and /generate/
          -- functions. Instead, we just try a few different categories.
        , map (\f -> Some $ ClassifiedGen CC_Fun (ignoreSize $ pure f)) [
              -- Parametrically polymorphic function
              unsafeCoerce (id    :: Int -> Int)
            , unsafeCoerce (const :: Int -> Bool -> Int)
              -- Ad-hoc polymorphic function
            , unsafeCoerce (negate :: Int -> Int)
            , unsafeCoerce ((+)    :: Int -> Int -> Int)
              -- Partial application
            , unsafeCoerce (const 1 :: Bool -> Int)
            , unsafeCoerce ((+)   1 :: Int -> Int)
            ]
        ]

    -- Compound
    --
    -- These are only used if @sz > 0@.
    compound :: [Maybe (Gen (Some ClassifiedGen))]
    compound = [
          -- Lists
          --
          -- We have to be careful not to generate @[Char]@, because this is
          -- inferred as @String@
          guard (typSz >= 1) >> (return $ do
              a <- arbitraryClassifiedGen (typSz - 1)
              genMaybeF
                (\case FJust CC_Char -> CC_String
                       c             -> CC_List c)
                (return [])
                (\gen -> SizedGen $ \valSz -> do
                   -- Pick number of list elements (don't generate empty list)
                   n <- choose (1, 5)

                   -- Then divide total size of each list element
                   vectorOf n (runSized (valSz `div` n) gen)
                )
                a
            )

          -- Maybe
        , guard (typSz >= 1) >> (return $ do
              a <- arbitraryClassifiedGen (typSz - 1)
              genMaybeF CC_Maybe (return Nothing) (fmap Just) a
            )

          -- Either
        , guard (typSz >= 2) >> (return $ do
              a <- arbitraryClassifiedGen (typSz `div` 2)
              b <- arbitraryClassifiedGen (typSz `div` 2)
              genEitherF CC_Either (fmap Left) (fmap Right) a b
            )

            -- User-defined
        , guard (typSz >= 1) >> (return $
              arbitraryClassifiedGen (typSz - 1) >>=
              genMaybeF
                CC_User_NonRec
                (NR1 <$> arbitrary)
                (\gen -> SizedGen $ \valSz ->
                    NR2 <$> runSized valSz gen <*> arbitrary
                )
            )

        , guard (typSz >= 1) >> (return $
              arbitraryClassifiedGen (typSz - 1) >>=
              genMaybeF
                CC_User_Rec
                (return RNil)
                (\gen -> SizedGen $ \valSz -> do
                  -- Similar strategy as for lists
                  n <- choose (1, 5)
                  recursiveFromList <$> vectorOf n (runSized (valSz `div` n) gen)
                )
            )

          -- Tuples
        , guard (typSz >= 2) >> (return $
              arbitraryTuple typSz $ \np ->
              case ( all_NP (hmap canShowClassifiedGen np)
                   , all_NP (hmap canEqClassifiedGen   np)
                   ) of
                (Dict, Dict) ->
                  return . Some $ ClassifiedGen {
                      genClassifier =
                        CC_Tuple (ConcreteClassifiers (hmap genClassifier np))
                    , classifiedGen = SizedGen $ \valSz -> do
                        let valSz' = valSz `div` lengthSList np
                        tupleFromNP <$>
                          hsequence(hmap (runSized valSz' . classifiedGen) np)
                    }
            )
        ]

    genMaybeF ::
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall x. MaybeF ConcreteClassifier x -> ConcreteClassifier (f x))
      -> Gen (f Void)
      -> (forall x. SizedGen x -> SizedGen (f x))
      -> Some ClassifiedGen -> Gen (Some ClassifiedGen)
    genMaybeF cc genNothing genJust (Some (ClassifiedGen cA genA)) =
        elements [
            Some $ ClassifiedGen (cc FNothing)   (ignoreSize $ genNothing)
          , Some $ ClassifiedGen (cc (FJust cA)) (genJust genA)
          ]

    genEitherF ::
         ( forall x y. (Show x, Show y) => Show (f x y)
         , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
         )
      => (forall x y. EitherF ConcreteClassifier x y -> ConcreteClassifier (f x y))
      -> (forall x. SizedGen x -> SizedGen (f x Void))
      -> (forall y. SizedGen y -> SizedGen (f Void y))
      -> Some ClassifiedGen -- a
      -> Some ClassifiedGen -- b
      -> Gen (Some ClassifiedGen)
    genEitherF cc
               genLeft
               genRight
               (Some (ClassifiedGen cA genA))
               (Some (ClassifiedGen cB genB))
               =
        elements [
            Some $ ClassifiedGen (cc (FLeft  cA)) (genLeft  genA)
          , Some $ ClassifiedGen (cc (FRight cB)) (genRight genB)
          ]

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

         -- String types

         C_String      -> ()
         C_BS_Strict   -> ()
         C_BS_Lazy     -> ()
         C_BS_Short    -> ()
         C_Text_Strict -> ()
         C_Text_Lazy   -> ()

         -- Compound

         C_Maybe{}  -> ()
         C_Either{} -> ()
         C_List{}   -> ()
         C_Tuple{}  -> ()

         -- Reference cells

         C_STRef -> ()
         C_TVar  -> ()
         C_MVar  -> ()

         -- Functions

         C_Fun -> ()

         -- User-defined

         C_Custom{} -> ()

         -- We don't generate values that we cannot classify

         C_Unknown -> ()

-- | Generate arbitrary tuple size
arbitraryTuple :: forall r.
     Int -- ^ Maximum type size (should be at least 2)
  -> (forall xs.
           (SListI xs, IsValidSize (Length xs))
        => NP ClassifiedGen xs -> Gen r
     )
  -> Gen r
arbitraryTuple = \typSz k -> do
    tupleSz <- choose (2, min typSz 62)
    let typSz' = typSz `div` tupleSz
    case toValidSize tupleSz of
      Nothing ->
        error "arbitraryTuple: impossible, this is a valid tuple size"
      Just (Some valid@(ValidSize n _)) ->
        go typSz' n $ \(np :: NP ClassifiedGen xs) ->
           case liftValidSize (valid :: ValidSize (Length xs))
             of Dict -> k np
  where
    go :: Int
       -> Sing (n :: Nat)
       -> (forall xs.
                (SListI xs, Length xs ~ n)
             => NP ClassifiedGen xs -> Gen r
          )
       -> Gen r
    go _      SZ     k = k Nil
    go typSz' (SS n) k = do
        Some c <- arbitraryClassifiedGen typSz'
        go typSz' n $ \cs -> k (c :* cs)

instance Arbitrary (Some Value) where
  arbitrary = sized $ \sz -> do
      -- @sz@ will range from 0..100, but we don't want to generate types that
      -- large
      Some (ClassifiedGen cc gen) <- arbitraryClassifiedGen (sz `div` 10)

      -- For the values however we want to be able to generate larger trees
      Some . Value cc <$> runSized sz gen

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
