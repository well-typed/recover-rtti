{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Debug.RecoverRTTI.Classify (
    -- * Classification
    classify
    -- * User-defined types
  , Classified(..)
  , fromUserDefined
    -- * Showing values
  , anythingToString
  , anythingToShowS
  , canShowPrim
  , canShowClassified
  , canShowClassified_
  ) where

import Control.Monad
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans (lift)
import Data.Foldable qualified as Foldable
import Data.HashMap.Internal.Array qualified as HashMap (Array)
import Data.HashMap.Internal.Array qualified as HashMap.Array
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Primitive.Array qualified as Prim (Array)
import Data.Sequence (Seq)
import Data.SOP
import Data.SOP.Dict
import Data.Vector qualified as Vector.Boxed
import GHC.Exts.Heap (Closure)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

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
      (inKnownModule DataByteStringInternal      -> Just "BS")    -> return $ mustBe $ C_Prim C_BS_Strict
      (inKnownModule DataByteStringLazyInternal  -> Just "Empty") -> return $ mustBe $ C_Prim C_BS_Lazy
      (inKnownModule DataByteStringLazyInternal  -> Just "Chunk") -> return $ mustBe $ C_Prim C_BS_Lazy
#if !MIN_VERSION_bytestring(0,12,0)
      (inKnownModule DataByteStringShortInternal -> Just "SBS")   -> return $ mustBe $ C_Prim C_BS_Short
#endif

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
        return $ mustBe C_Maybe
      (inKnownModule GhcMaybe -> Just "Just") ->
        return $ mustBe C_Maybe

      -- Either
      (inKnownModule DataEither -> Just "Left") ->
        return $ mustBe C_Either
      (inKnownModule DataEither -> Just "Right") ->
        return $ mustBe C_Either

      -- Lists (this includes the 'String' case)
      (inKnownModule GhcTypes -> Just "[]") ->
        mustBe <$> classifyList (unsafeCoerce x)
      (inKnownModule GhcTypes -> Just ":") ->
        mustBe <$> classifyList (unsafeCoerce x)

      -- Ratio
      (inKnownModule GhcReal -> Just ":%") ->
        return $ mustBe C_Ratio

      -- Set
      (inKnownModule DataSetInternal -> Just "Tip") ->
        return $ mustBe C_Set
      (inKnownModule DataSetInternal -> Just "Bin") ->
        return $ mustBe C_Set

      -- Map
      (inKnownModule DataMapInternal -> Just "Tip") ->
        return $ mustBe C_Map
      (inKnownModule DataMapInternal -> Just "Bin") ->
        return $ mustBe C_Map

      -- IntSet
      (inKnownModule DataIntSetInternal -> Just "Bin") ->
        return $ mustBe $ C_Prim C_IntSet
      (inKnownModule DataIntSetInternal -> Just "Tip") ->
        return $ mustBe $ C_Prim C_IntSet
      (inKnownModule DataIntSetInternal -> Just "Nil") ->
        return $ mustBe $ C_Prim C_IntSet

      -- IntMap
      (inKnownModule DataIntMapInternal -> Just "Nil") ->
        return $ mustBe C_IntMap
      (inKnownModule DataIntMapInternal -> Just "Tip") ->
        return $ mustBe C_IntMap
      (inKnownModule DataIntMapInternal -> Just "Bin") ->
        return $ mustBe C_IntMap

      -- Sequence
      (inKnownModule DataSequenceInternal -> Just "EmptyT") ->
        mustBe <$> classifySequence (unsafeCoerce x)
      (inKnownModule DataSequenceInternal -> Just "Single") ->
        mustBe <$> classifySequence (unsafeCoerce x)
      (inKnownModule DataSequenceInternal -> Just "Deep") ->
        mustBe <$> classifySequence (unsafeCoerce x)

      -- Tree
      (inKnownModule DataTree -> Just "Node") ->
        return $ mustBe C_Tree

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

#if !MIN_VERSION_primitive(0,8,0)
      (inKnownModule DataPrimitiveByteArray -> Just "ByteArray") ->
        return $ mustBe $ C_Prim C_ByteArray
      (inKnownModule DataPrimitiveByteArray -> Just "MutableByteArray") ->
        return $ mustBe $ C_Prim C_MutableByteArray
#else
      (inKnownModule DataArrayByte -> Just "ByteArray") ->
        return $ mustBe $ C_Prim C_ByteArray
      (inKnownModule DataArrayByte -> Just "MutableByteArray") ->
        return $ mustBe $ C_Prim C_MutableByteArray
#endif

      -- Boxed vectors
      (inKnownModule DataVector -> Just "Vector") ->
        mustBe <$> classifyBoxedVector (unsafeCoerce x)

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
  List elements

  We add a special case for @[Char]@, so that that @show@ will use the
  (overlapped) instance for @String@ instead of the general instance for @[a]@.
  We cannot recognize this for empty lists, but printing those as @[]@ is OK. We
  must do this not only for lists proper, but also for datatypes where the
  'Show' instance piggy-backs on the instance for lists.

  We must however be careful with list(-like)s of 'Any', where the first
  character happens tob be a 'Char'. We therefore check /all/ elements.
-------------------------------------------------------------------------------}

type ClassifyListLike f = forall a. f a -> ExceptT Closure IO (Classifier (f a))

classifyListLike :: forall f.
     (forall a. ClassifyListElem a -> Classifier_ IsUserDefined (f a))
  -> (forall a. f a -> [a])
  -> ClassifyListLike f
classifyListLike cf toList = \xs ->
    -- If the container is empty, we have a choice. It is however /probably/
    -- more confusing to display a non-string as a string (albeit empty) than
    -- the other way around.
    case toList xs of
      []    -> return $ mustBe $ cf C_List_Deferred
      x:xs' -> go (x:xs')
  where
    -- Check every element
    --
    -- Precondition: all elements /already/ considered must all be 'Char'.
    go :: [a] -> ExceptT Closure IO (Classifier (f a))
    go []     = return $ mustBe $ cf C_List_Char
    go (x:xs) = do
        cx <- classifyIO x
        case cx of
          C_Prim C_Char -> go xs
          _otherwise    -> return $ mustBe $ cf C_List_Deferred

classifyBoxedVector :: ClassifyListLike Vector.Boxed.Vector
classifyHMArray     :: ClassifyListLike HashMap.Array
classifyPrimArray   :: ClassifyListLike Prim.Array
classifyList        :: ClassifyListLike []
classifySequence    :: ClassifyListLike Seq

classifyBoxedVector = classifyListLike C_Vector_Boxed Foldable.toList
classifyHMArray     = classifyListLike C_HM_Array     HashMap.Array.toList
classifyPrimArray   = classifyListLike C_Prim_Array   Foldable.toList
classifyList        = classifyListLike C_List         id
classifySequence    = classifyListLike C_Sequence     Foldable.toList

{-------------------------------------------------------------------------------
  HashMap

  'HashSet' and 'HashMap' cannot be distinguished from just looking at the heap
  , because 'HashSet' is a newtype around 'HashMap'. Instead, we classify a
  'HashMap' with value type @()@ as a 'HashSet'.

  In princple we /should/ look at all elements (like we do for list-like
  containers). However, it is extremely unlikely that we will a HashMap of
  'Any', where the first value happens to be of type @()@. Moreover, if it
  /does/ happen, the only consequence is that we omit the values (printing the
  keys as a set); no segfault can happen (and we test this). We therefore look
  simply at the first element only.
-------------------------------------------------------------------------------}

-- | Classify 'HashMap'
--
-- We try to recognize 'HashSet', if we can.
classifyHashMap ::
     HashMap a b
  -> ExceptT Closure IO (Classifier (HashMap a b))
classifyHashMap xs =
    case HashMap.elems xs of
      []  -> return $ mustBe C_HashMap
      x:_ -> aux <$> classifyIO x
  where
    aux :: Classifier b -> Classifier (HashMap a b)
    aux (C_Prim C_Unit) = mustBe C_HashSet
    aux _otherwise      = mustBe C_HashMap

{-------------------------------------------------------------------------------
  Classifying tuples
-------------------------------------------------------------------------------}

classifyTuple ::
     (SListI xs, IsValidSize (Length xs))
  => NP (K Box) xs
  -> ExceptT Closure IO (Classifier (WrappedTuple xs))
classifyTuple ptrs = do
    C_Tuple . Classifiers_ <$> hsequence' (hmap aux ptrs)
  where
    aux :: K Box a -> (ExceptT Closure IO :.: Classifier) a
    aux (K (Box x)) = Comp $ classifyIO (unsafeCoerce x)

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
anythingToString x = anythingToShowS 0 x ""

-- | Generalization of 'anythingToString' with user-specified precedence
anythingToShowS :: forall a. Int -> a -> ShowS
anythingToShowS p x =
    case classify x of
      Left  closure    -> showsPrec p closure
      Right classifier -> case canShowClassified classifier of
                            Dict -> showsPrec p x


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

instance Show Deferred where
  showsPrec = anythingToShowS