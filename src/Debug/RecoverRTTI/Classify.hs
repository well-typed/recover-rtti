{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Debug.RecoverRTTI.Classify (
    -- * Classification
    classify
    -- * User-defined types
  , Classified(..)
  , fromUserDefined
    -- * Showing values
  , anythingToString
  , canShowPrim
  , canShowClassified
  , canShowClassified_
    -- * Patterns for common shapes of 'Elems' (exported for the tests)
  , pattern ElemK
  , pattern ElemU
  , pattern ElemKK
  , pattern ElemUU
  , pattern ElemKU
  , pattern ElemUK
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.HashMap.Lazy (HashMap)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.SOP
import Data.SOP.Dict
import Data.Tree (Tree)
import Data.Void
import GHC.Exts.Heap (Closure)
import GHC.Real
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Foldable               as Foldable
import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.Map                    as Map
import qualified Data.Primitive.Array        as Prim.Array
import qualified Data.Primitive.Array        as Prim (Array)
import qualified Data.Tree                   as Tree
import qualified Data.Vector                 as Vector.Boxed

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Constraint
import Debug.RecoverRTTI.FlatClosure
import Debug.RecoverRTTI.Modules
import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Wrappers

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

classifyIO :: a -> ExceptT Closure IO (Classifier a)
classifyIO x = do
    closure <- lift $ getBoxedClosureData (asBox x)
    case closure of
      --
      -- Primitive (ghc-prim)
      --

      -- GHC.Types
      (inKnownModule GhcTypes -> Just "True")  -> return $ mustBe $ C_Prim C_Bool
      (inKnownModule GhcTypes -> Just "False") -> return $ mustBe $ C_Prim C_Bool
      (inKnownModule GhcTypes -> Just "C#")    -> return $ mustBe $ C_Prim C_Char
      (inKnownModule GhcTypes -> Just "D#")    -> return $ mustBe $ C_Prim C_Double
      (inKnownModule GhcTypes -> Just "F#")    -> return $ mustBe $ C_Prim C_Float
      (inKnownModule GhcTypes -> Just "I#")    -> return $ mustBe $ C_Prim C_Int
      (inKnownModule GhcTypes -> Just "LT")    -> return $ mustBe $ C_Prim C_Ordering
      (inKnownModule GhcTypes -> Just "GT")    -> return $ mustBe $ C_Prim C_Ordering
      (inKnownModule GhcTypes -> Just "EQ")    -> return $ mustBe $ C_Prim C_Ordering
      (inKnownModule GhcTypes -> Just "W#")    -> return $ mustBe $ C_Prim C_Word

      -- GHC.Tuple
      (inKnownModule GhcTuple -> Just "()") -> return $ mustBe $ C_Prim C_Unit

      -- GHC.Int
      (inKnownModule GhcInt -> Just "I8#")  -> return $ mustBe $ C_Prim C_Int8
      (inKnownModule GhcInt -> Just "I16#") -> return $ mustBe $ C_Prim C_Int16
      (inKnownModule GhcInt -> Just "I32#") -> return $ mustBe $ C_Prim C_Int32
      (inKnownModule GhcInt -> Just "I64#") -> return $ mustBe $ C_Prim C_Int64

      -- GHC.Integer
      (inKnownModule GhcIntegerType -> Just "S#")  -> return $ mustBe $ C_Prim C_Integer
      (inKnownModule GhcIntegerType -> Just "Jp#") -> return $ mustBe $ C_Prim C_Integer
      (inKnownModule GhcIntegerType -> Just "Jn#") -> return $ mustBe $ C_Prim C_Integer
      (inKnownModule GhcNumInteger  -> Just "IS")  -> return $ mustBe $ C_Prim C_Integer
      (inKnownModule GhcNumInteger  -> Just "IP")  -> return $ mustBe $ C_Prim C_Integer
      (inKnownModule GhcNumInteger  -> Just "IN")  -> return $ mustBe $ C_Prim C_Integer

      -- GHC.Word
      (inKnownModule GhcWord -> Just "W8#")  -> return $ mustBe $ C_Prim C_Word8
      (inKnownModule GhcWord -> Just "W16#") -> return $ mustBe $ C_Prim C_Word16
      (inKnownModule GhcWord -> Just "W32#") -> return $ mustBe $ C_Prim C_Word32
      (inKnownModule GhcWord -> Just "W64#") -> return $ mustBe $ C_Prim C_Word64

      --
      -- String types
      --

      -- bytestring
      --
      -- bytestring changed from PS to BS in version 0.11
      (inKnownModule DataByteStringInternal      -> Just "PS")    -> return $ mustBe $ C_Prim C_BS_Strict
      (inKnownModule DataByteStringInternal      -> Just "BS")    -> return $ mustBe $ C_Prim C_BS_Strict
      (inKnownModule DataByteStringLazyInternal  -> Just "Empty") -> return $ mustBe $ C_Prim C_BS_Lazy
      (inKnownModule DataByteStringLazyInternal  -> Just "Chunk") -> return $ mustBe $ C_Prim C_BS_Lazy
      (inKnownModule DataByteStringShortInternal -> Just "SBS")   -> return $ mustBe $ C_Prim C_BS_Short

      -- text
      (inKnownModule DataTextInternal     -> Just "Text")  -> return $ mustBe $ C_Prim C_Text_Strict
      (inKnownModule DataTextInternalLazy -> Just "Chunk") -> return $ mustBe $ C_Prim C_Text_Lazy
      (inKnownModule DataTextInternalLazy -> Just "Empty") -> return $ mustBe $ C_Prim C_Text_Lazy

      --
      -- Aeson
      --

      (inKnownModule DataAesonTypesInternal -> Just "Object") -> return $ mustBe $ C_Prim C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Array")  -> return $ mustBe $ C_Prim C_Value
      (inKnownModule DataAesonTypesInternal -> Just "String") -> return $ mustBe $ C_Prim C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Number") -> return $ mustBe $ C_Prim C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Bool")   -> return $ mustBe $ C_Prim C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Null")   -> return $ mustBe $ C_Prim C_Value

      --
      -- Compound (ghc-prim)
      --

      -- Maybe
      (inKnownModule GhcMaybe -> Just "Nothing") ->
        mustBe <$> classifyMaybe (unsafeCoerce x)
      (inKnownModule GhcMaybe -> Just "Just") ->
        mustBe <$> classifyMaybe (unsafeCoerce x)

      -- Either
      (inKnownModule DataEither -> Just "Left") ->
        mustBe <$> classifyEither (unsafeCoerce x)
      (inKnownModule DataEither -> Just "Right") ->
        mustBe <$> classifyEither (unsafeCoerce x)

      -- Lists (this includes the 'String' case)
      (inKnownModule GhcTypes -> Just "[]") ->
        mustBe <$> classifyList (unsafeCoerce x)
      (inKnownModule GhcTypes -> Just ":") ->
        mustBe <$> classifyList (unsafeCoerce x)

      -- Ratio
      (inKnownModule GhcReal -> Just ":%") ->
        mustBe <$> classifyRatio (unsafeCoerce x)

      -- Set
      (inKnownModule DataSetInternal -> Just "Tip") ->
        mustBe <$> classifySet (unsafeCoerce x)
      (inKnownModule DataSetInternal -> Just "Bin") ->
        mustBe <$> classifySet (unsafeCoerce x)

      -- Map
      (inKnownModule DataMapInternal -> Just "Tip") ->
        mustBe <$> classifyMap (unsafeCoerce x)
      (inKnownModule DataMapInternal -> Just "Bin") ->
        mustBe <$> classifyMap (unsafeCoerce x)

      -- IntSet
      (inKnownModule DataIntSetInternal -> Just "Bin") ->
        return $ mustBe $ C_Prim C_IntSet
      (inKnownModule DataIntSetInternal -> Just "Tip") ->
        return $ mustBe $ C_Prim C_IntSet
      (inKnownModule DataIntSetInternal -> Just "Nil") ->
        return $ mustBe $ C_Prim C_IntSet

      -- IntMap
      (inKnownModule DataIntMapInternal -> Just "Nil") ->
        mustBe <$> classifyIntMap (unsafeCoerce x)
      (inKnownModule DataIntMapInternal -> Just "Tip") ->
        mustBe <$> classifyIntMap (unsafeCoerce x)
      (inKnownModule DataIntMapInternal -> Just "Bin") ->
        mustBe <$> classifyIntMap (unsafeCoerce x)

      -- Sequence
      (inKnownModule DataSequenceInternal -> Just "EmptyT") ->
        mustBe <$> classifySequence (unsafeCoerce x)
      (inKnownModule DataSequenceInternal -> Just "Single") ->
        mustBe <$> classifySequence (unsafeCoerce x)
      (inKnownModule DataSequenceInternal -> Just "Deep") ->
        mustBe <$> classifySequence (unsafeCoerce x)

      -- Tree
      (inKnownModule DataTree -> Just "Node") ->
        mustBe <$> classifyTree (unsafeCoerce x)

      -- Tuples (of size 2..62)
      (inKnownModuleNested GhcTuple -> Just (
            isTuple       -> Just (Some validSize@(ValidSize sz _))
          , verifySize sz -> Just (VerifiedSize ptrs)
          )) ->
        case liftValidSize validSize of
          Dict -> mustBe <$> classifyTuple ptrs

      -- HashMap
      --
      -- This could also be a HashSet, which is a newtype around a HashMap;
      -- we distinguish in 'classifyHashMap'.
      (inKnownModule DataHashMapInternal -> Just "Empty") ->
        mustBe <$> classifyHashMap (unsafeCoerce x)
      (inKnownModule DataHashMapInternal -> Just "BitmapIndexed") ->
        mustBe <$> classifyHashMap (unsafeCoerce x)
      (inKnownModule DataHashMapInternal -> Just "Leaf") ->
        mustBe <$> classifyHashMap (unsafeCoerce x)
      (inKnownModule DataHashMapInternal -> Just "Full") ->
        mustBe <$> classifyHashMap (unsafeCoerce x)
      (inKnownModule DataHashMapInternal -> Just "Collision") ->
        mustBe <$> classifyHashMap (unsafeCoerce x)

      -- HashMap's internal Array type
      (inKnownModule DataHashMapInternalArray -> Just "Array") ->
        mustBe <$> classifyHMArray (unsafeCoerce x)

      -- Arrays from @primitive@
      (inKnownModule DataPrimitiveArray -> Just "Array") ->
        mustBe <$> classifyPrimArray (unsafeCoerce x)
      (inKnownModule DataPrimitiveArray -> Just "MutableArray") ->
        return $ mustBe $ C_Prim C_Prim_ArrayM

      -- Boxed vectors
      (inKnownModule DataVector -> Just "Vector") ->
        mustBe <$> classifyVectorBoxed (unsafeCoerce x)

      -- Storable vectors
      (inKnownModule DataVectorStorable -> Just "Vector") ->
        return $ mustBe $ C_Prim C_Vector_Storable
      (inKnownModule DataVectorStorableMutable -> Just "MVector") ->
        return $ mustBe $ C_Prim C_Vector_StorableM

      -- Primitive vectors
      (inKnownModule DataVectorPrimitive -> Just "Vector") ->
        return $ mustBe $ C_Prim C_Vector_Primitive
      (inKnownModule DataVectorPrimitiveMutable -> Just "MVector") ->
        return $ mustBe $ C_Prim C_Vector_PrimitiveM

      --
      -- Reference cells
      --

      (inKnownModule GhcSTRef    -> Just "STRef") -> return $ mustBe $ C_Prim C_STRef
      (inKnownModule GhcMVar     -> Just "MVar")  -> return $ mustBe $ C_Prim C_MVar
      (inKnownModule GhcConcSync -> Just "TVar")  -> return $ mustBe $ C_Prim C_TVar

      --
      -- Functions
      --

      FunClosure {} -> return $ mustBe $ C_Prim C_Fun

      --
      -- User defined
      --

      ConstrClosure {} ->
        return $ mustBe $ C_Other (IsUserDefined (unsafeCoerce x))

      --
      -- Classification failed
      --

      OtherClosure other -> ExceptT $ return (Left other)

mustBe :: Classifier_ o b -> Classifier_ o a
mustBe = unsafeCoerce

-- | Classify a value
--
-- Given a value of some unknown type @a@ and a classifier @Classifier a@,
-- it should be sound to coerce the value to the type indicated by the
-- classifier.
--
-- This is also the reason not all values can be classified; in particular,
-- we cannot classify values of unlifted types, as for these types coercion
-- does not work (this would result in a ghc runtime crash).
classify :: a -> Either Closure (Classifier a)
classify = unsafePerformIO . runExceptT . classifyIO

{-------------------------------------------------------------------------------
  Classification for compound types
-------------------------------------------------------------------------------}

classifyMaybe :: Maybe a -> ExceptT Closure IO (Classifier (Maybe a))
classifyMaybe = classifyFoldable C_Maybe

classifyEither ::
     Either a b
  -> ExceptT Closure IO (Classifier (Either a b))
classifyEither x =
    case x of
      Left  x' -> (mustBe . C_Either . ElemKU)  <$> classifyIO x'
      Right y' -> (mustBe . C_Either . ElemUK) <$> classifyIO y'

classifyList :: [a] -> ExceptT Closure IO (Classifier [a])
classifyList = classifyFoldable c_list
  where
    -- We special case for @String@, so that @show@ will use the (overlapped)
    -- instance for @String@ instead of the general instance for @[a]@
    c_list :: Elems o '[x] -> Classifier_ o [x]
    c_list (ElemK (C_Prim C_Char)) = C_Prim C_String
    c_list c = C_List c

classifyRatio :: Ratio a -> ExceptT Closure IO (Classifier (Ratio a))
classifyRatio (x' :% _) = mustBe . C_Ratio . ElemK <$> classifyIO x'

classifySet :: Set a -> ExceptT Closure IO (Classifier (Set a))
classifySet = classifyFoldable C_Set

classifyMap :: Map a b -> ExceptT Closure IO (Classifier (Map a b))
classifyMap = classifyFoldablePair C_Map Map.toList

classifyIntMap :: IntMap a -> ExceptT Closure IO (Classifier (IntMap a))
classifyIntMap = classifyFoldable C_IntMap

classifySequence :: Seq a -> ExceptT Closure IO (Classifier (Seq a))
classifySequence = classifyFoldable C_Sequence

classifyTree :: Tree a -> ExceptT Closure IO (Classifier (Tree a))
classifyTree (Tree.Node x' _) = mustBe . C_Tree . ElemK <$> classifyIO x'

classifyHashMap :: HashMap a b -> ExceptT Closure IO (Classifier (HashMap a b))
classifyHashMap = classifyFoldablePair c_hashmap HashMap.toList
  where
    -- HashSet is a newtype around HashMap
    c_hashmap :: Elems o '[x, y] -> Classifier_ o (HashMap x y)
    c_hashmap (ElemKK c (C_Prim C_Unit)) = mustBe $ C_HashSet (ElemK c)
    c_hashmap c = C_HashMap c

classifyHMArray ::
     HashMap.Array a
  -> ExceptT Closure IO (Classifier (HashMap.Array a))
classifyHMArray =
    classifyArrayLike
      C_HM_Array
      HashMap.Array.length
      (`HashMap.Array.index` 0)

classifyPrimArray ::
     Prim.Array a
  -> ExceptT Closure IO (Classifier (Prim.Array a))
classifyPrimArray =
    classifyArrayLike
      C_Prim_Array
      Prim.Array.sizeofArray
      (`Prim.Array.indexArray` 0)

classifyVectorBoxed ::
     Vector.Boxed.Vector a
  -> ExceptT Closure IO (Classifier (Vector.Boxed.Vector a))
classifyVectorBoxed =
    classifyArrayLike
      C_Vector_Boxed
      Vector.Boxed.length
      Vector.Boxed.head

classifyTuple ::
     (SListI xs, IsValidSize (Length xs))
  => NP (K Box) xs
  -> ExceptT Closure IO (Classifier (WrappedTuple xs))
classifyTuple ptrs = do
    cs <- hsequence' (hmap aux ptrs)
    return $ C_Tuple (Elems (hmap Elem cs))
  where
    aux :: K Box a -> (ExceptT Closure IO :.: Classifier) a
    aux (K (Box x)) = Comp $ classifyIO (unsafeCoerce x)

{-------------------------------------------------------------------------------
  Helper functions for defining classifiers
-------------------------------------------------------------------------------}

classifyFoldable ::
     Foldable f
  => (forall o x. Elems o '[x] -> Classifier_ o (f x))
  -> f a -> ExceptT Closure IO (Classifier (f a))
classifyFoldable cc x =
    case Foldable.toList x of
      []   -> return $ mustBe $ cc ElemU
      x':_ -> mustBe . cc . ElemK <$> classifyIO x'

classifyFoldablePair ::
     (forall o x y. Elems o '[x, y] -> Classifier_ o (f x y))
  -> (f a b -> [(a, b)])
  -> f a b -> ExceptT Closure IO (Classifier (f a b))
classifyFoldablePair cc toList x =
    case toList x of
      []         -> return $ mustBe $ cc ElemUU
      (x', y'):_ -> (\ca cb -> mustBe $ cc (ElemKK ca cb))
                       <$> classifyIO x'
                       <*> classifyIO y'

classifyArrayLike ::
     (forall o x. Elems o '[x] -> Classifier_ o (f x))
  -> (f a -> Int)  -- ^ Get the length of the array
  -> (f a -> a)    -- ^ Get the first element (provided the array is not empty)
  -> f a -> ExceptT Closure IO (Classifier (f a))
classifyArrayLike cc getLen getFirst x =
    if getLen x == 0
      then return $ mustBe $ cc ElemU
      else do
        let x' = getFirst x
        mustBe . cc . ElemK <$> classifyIO x'

{-------------------------------------------------------------------------------
  Patterns for common shapes of 'Elems'

  This is mostly useful internally; we export these only for the benefit of the
  QuickCheck generator. Most other code can treat the all types uniformly.

  We distinguish between which elements are (K)nown and which (U)nknown
-------------------------------------------------------------------------------}

pattern ElemK :: Classifier_ o a -> Elems o '[a]
pattern ElemK c = Elems (Elem c :* Nil)

pattern ElemU :: Elems o '[Void]
pattern ElemU = Elems (NoElem :* Nil)

pattern ElemKK :: Classifier_ o a -> Classifier_ o b -> Elems o '[a, b]
pattern ElemKK ca cb = Elems (Elem ca :* Elem cb :* Nil)

pattern ElemUU :: Elems o '[Void, Void]
pattern ElemUU = Elems (NoElem :* NoElem :* Nil)

pattern ElemKU :: Classifier_ o a -> Elems o '[a, Void]
pattern ElemKU c = Elems (Elem c :* NoElem :* Nil)

pattern ElemUK :: Classifier_ o b -> Elems o '[Void, b]
pattern ElemUK c = Elems (NoElem :* Elem c :* Nil)

{-------------------------------------------------------------------------------
  Recognizing tuples
-------------------------------------------------------------------------------}

isTuple :: String -> Maybe (Some ValidSize)
isTuple typ = do
    (a, xs, z) <- dropEnds typ
    guard $ a == '(' && all (== ',') xs && z == ')'
    toValidSize (length xs + 1)

{-------------------------------------------------------------------------------
  Classify constructor arguments
-------------------------------------------------------------------------------}

-- | Bundle a value with its classifier
data Classified a = Classified (Classifier a) a

-- | Classify the arguments to the constructor
--
-- Additionally returns the constructor name itself.
fromUserDefined :: UserDefined -> (String, [Some Classified])
fromUserDefined = \(UserDefined x) -> unsafePerformIO $ go x
  where
    go :: x -> IO (String, [Some Classified])
    go x = do
        closure <- getBoxedClosureData (asBox x)
        case closure of
          ConstrClosure {name, ptrArgs} ->
            (name,) <$> goArgs [] ptrArgs
          _otherwise ->
            error $ "elimUserDefined: unexpected closure: "
                 ++ show closure

    goArgs :: [Some Classified] -> [Box] -> IO [Some Classified]
    goArgs acc []         = return (reverse acc)
    goArgs acc (Box b:bs) = do
        mc <- runExceptT $ classifyIO b
        case mc of
          Right c -> goArgs (Some (Classified c (unsafeCoerce b)) : acc) bs
          Left  _ -> goArgs                                         acc  bs

{-------------------------------------------------------------------------------
  Show

  Showing values is mutually recursive with classification: when we show a
  value classified as @UserDefined@, we recursively classify the nested values
  /when/ we show the value.
-------------------------------------------------------------------------------}

-- | Show any value
--
-- This shows any value, as long as it's not unlifted. The result should be
-- equal to show instances, with the following caveats:
--
-- * User-defined types (types not explicitly known to this library) with a
--   /custom/ Show instance will still be showable, but the result will be
--   what the /derived/ show instance would have done.
-- * Record field names are not known at runtime, so they are not shown.
-- * UNPACKed data is not visible to this library (if you compile with @-O0@
--   @ghc@ will not unpack data, so that might be a workaround if necessary).
--
-- If classification fails, we show the actual closure.
anythingToString :: forall a. a -> String
anythingToString x =
    case classify x of
      Left  closure    -> show closure
      Right classifier -> case canShowClassified classifier of
                            Dict -> show x

deriving instance Show (Some Classified)

instance Show (Classified a) where
  showsPrec p (Classified c x) = showParen (p >= 11) $
      case canShowClassified c of
        Dict ->
            showString "Classified "
          . showsPrec 11 c
          . showString " "
          . showsPrec 11 x

-- | Show the classified value (without the classifier)
showClassifiedValue :: Int -> Classified a -> ShowS
showClassifiedValue p (Classified c x) =
    case canShowClassified c of
      Dict -> showsPrec p x

canShowClassified :: Classifier a -> Dict Show a
canShowClassified = canShowClassified_ showOther
  where
    showOther :: IsUserDefined a -> Dict Show a
    showOther (IsUserDefined _) = Dict

canShowPrim :: PrimClassifier a -> Dict Show a
canShowPrim = primSatisfies

canShowClassified_ :: forall o.
     (forall a. o a -> Dict Show a)
  -> (forall a. Classifier_ o a -> Dict Show a)
canShowClassified_ = classifiedSatisfies

instance Show UserDefined where
  showsPrec p x =
      case args of
        [] -> showString constrName
        xs -> showParen (p >= 11)
            . (showString constrName .)
            . foldl (.) id
            . map (\(Some x') -> showString " " . showClassifiedValue 11 x')
            $ xs
    where
      (constrName, args) = fromUserDefined x
