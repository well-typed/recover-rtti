{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Debug.RecoverRTTI.Classify (
    -- * Classification
    classify
  , classified
  , fromUserDefined
    -- * Showing values
  , anythingToString
  , canShowClassified
  ) where

import Control.Monad.Except
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.SOP
import Data.SOP.Dict
import Data.Tree (Tree)
import GHC.Real
import GHC.Stack
import GHC.Exts.Heap (Closure)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IntMap   as IntMap
import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
import qualified Data.Tree     as Tree

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.FlatClosure
import Debug.RecoverRTTI.Modules
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.TypeLevel
import Debug.RecoverRTTI.UserDefined
import Debug.RecoverRTTI.Util

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
      (inKnownModule GhcTypes -> Just "True")  -> return $ mustBe C_Bool
      (inKnownModule GhcTypes -> Just "False") -> return $ mustBe C_Bool
      (inKnownModule GhcTypes -> Just "C#")    -> return $ mustBe C_Char
      (inKnownModule GhcTypes -> Just "D#")    -> return $ mustBe C_Double
      (inKnownModule GhcTypes -> Just "F#")    -> return $ mustBe C_Float
      (inKnownModule GhcTypes -> Just "I#")    -> return $ mustBe C_Int
      (inKnownModule GhcTypes -> Just "LT")    -> return $ mustBe C_Ordering
      (inKnownModule GhcTypes -> Just "GT")    -> return $ mustBe C_Ordering
      (inKnownModule GhcTypes -> Just "EQ")    -> return $ mustBe C_Ordering
      (inKnownModule GhcTypes -> Just "W#")    -> return $ mustBe C_Word

      -- GHC.Tuple
      (inKnownModule GhcTuple -> Just "()") -> return $ mustBe C_Unit

      -- GHC.Int
      (inKnownModule GhcInt -> Just "I8#")  -> return $ mustBe C_Int8
      (inKnownModule GhcInt -> Just "I16#") -> return $ mustBe C_Int16
      (inKnownModule GhcInt -> Just "I32#") -> return $ mustBe C_Int32
      (inKnownModule GhcInt -> Just "I64#") -> return $ mustBe C_Int64

      -- GHC.Integer
      (inKnownModule GhcIntegerType -> Just "S#")  -> return $ mustBe C_Integer
      (inKnownModule GhcIntegerType -> Just "Jp#") -> return $ mustBe C_Integer
      (inKnownModule GhcIntegerType -> Just "Jn#") -> return $ mustBe C_Integer
      (inKnownModule GhcNumInteger  -> Just "IS")  -> return $ mustBe C_Integer
      (inKnownModule GhcNumInteger  -> Just "IP")  -> return $ mustBe C_Integer
      (inKnownModule GhcNumInteger  -> Just "IN")  -> return $ mustBe C_Integer

      -- GHC.Word
      (inKnownModule GhcWord -> Just "W8#")  -> return $ mustBe C_Word8
      (inKnownModule GhcWord -> Just "W16#") -> return $ mustBe C_Word16
      (inKnownModule GhcWord -> Just "W32#") -> return $ mustBe C_Word32
      (inKnownModule GhcWord -> Just "W64#") -> return $ mustBe C_Word64

      --
      -- String types
      --

      -- bytestring
      (inKnownModule DataByteStringInternal      -> Just "PS")    -> return $ mustBe C_BS_Strict
      (inKnownModule DataByteStringLazyInternal  -> Just "Empty") -> return $ mustBe C_BS_Lazy
      (inKnownModule DataByteStringLazyInternal  -> Just "Chunk") -> return $ mustBe C_BS_Lazy
      (inKnownModule DataByteStringShortInternal -> Just "SBS")   -> return $ mustBe C_BS_Short

      -- text
      (inKnownModule DataTextInternal     -> Just "Text")  -> return $ mustBe C_Text_Strict
      (inKnownModule DataTextInternalLazy -> Just "Chunk") -> return $ mustBe C_Text_Lazy
      (inKnownModule DataTextInternalLazy -> Just "Empty") -> return $ mustBe C_Text_Lazy

      --
      -- Aeson
      --

      (inKnownModule DataAesonTypesInternal -> Just "Object") -> return $ mustBe C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Array")  -> return $ mustBe C_Value
      (inKnownModule DataAesonTypesInternal -> Just "String") -> return $ mustBe C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Number") -> return $ mustBe C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Bool")   -> return $ mustBe C_Value
      (inKnownModule DataAesonTypesInternal -> Just "Null")   -> return $ mustBe C_Value

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
        return $ mustBe $ C_IntSet
      (inKnownModule DataIntSetInternal -> Just "Tip") ->
        return $ mustBe $ C_IntSet
      (inKnownModule DataIntSetInternal -> Just "Nil") ->
        return $ mustBe $ C_IntSet

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

      --
      -- Reference cells
      --

      (inKnownModule GhcSTRef    -> Just "STRef") -> return $ mustBe C_STRef
      (inKnownModule GhcMVar     -> Just "MVar")  -> return $ mustBe C_MVar
      (inKnownModule GhcConcSync -> Just "TVar")  -> return $ mustBe C_TVar

      --
      -- Functions
      --

      FunClosure {} -> return $ mustBe C_Fun

      --
      -- User defined
      --

      ConstrClosure {pkg, modl, name} ->
        elimKnownConstr (Constr pkg modl name) $ \p ->
        return $ mustBe (C_Custom p)

      --
      -- Classification failed
      --

      OtherClosure other -> ExceptT $ return (Left other)

mustBe :: Classifier b -> Classifier a
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
classifyMaybe x =
    case x of
      Nothing -> return $ mustBe $ C_Maybe FNothing
      Just x' -> do
        cx <- classifyIO x'
        return $ mustBe $ C_Maybe (FJust (Classified cx x'))

classifyEither :: Either a b -> ExceptT Closure IO (Classifier (Either a b))
classifyEither x =
    case x of
      Left x' -> do
        cx <- classifyIO x'
        return $ mustBe $ C_Either (FLeft (Classified cx x'))
      Right y' -> do
        cy <- classifyIO y'
        return $ mustBe $ C_Either (FRight (Classified cy y'))

classifyList :: [a] -> ExceptT Closure IO (Classifier [a])
classifyList x =
    case x of
      []   -> return $ mustBe $ C_List FNothing
      x':_ -> do
        cx <- classifyIO x'
        return $ case cx of
          C_Char     -> mustBe $ C_String
          _otherwise -> mustBe $ C_List (FJust (Classified cx x'))

classifyRatio :: Ratio a -> ExceptT Closure IO (Classifier (Ratio a))
classifyRatio (x' :% _) = do
    cx <- classifyIO x'
    return $ mustBe $ C_Ratio (Classified cx x')

classifySet :: Set a -> ExceptT Closure IO (Classifier (Set a))
classifySet x =
    case Set.lookupMin x of
      Nothing -> return $ mustBe $ C_Set FNothing
      Just x' -> do
        cx <- classifyIO x'
        return $ mustBe $ C_Set (FJust (Classified cx x'))

classifyMap :: Map a b -> ExceptT Closure IO (Classifier (Map a b))
classifyMap x =
   case Map.lookupMin x of
     Nothing       -> return $ mustBe $ C_Map FNothingPair
     Just (x', y') -> do
       cx <- classifyIO x'
       cy <- classifyIO y'
       return $ mustBe $ C_Map (FJustPair (Classified cx x') (Classified cy y'))

classifyIntMap :: IntMap a -> ExceptT Closure IO (Classifier (IntMap a))
classifyIntMap x =
    case IntMap.minView x of
      Nothing      -> return $ mustBe $ C_IntMap FNothing
      Just (x', _) -> do
        cx <- classifyIO x'
        return $ mustBe $ C_IntMap (FJust (Classified cx x'))

classifySequence :: Seq a -> ExceptT Closure IO (Classifier (Seq a))
classifySequence x =
    case Seq.viewl x of
      Seq.EmptyL  -> return $ mustBe $ C_Sequence FNothing
      x' Seq.:< _ -> do
        cx <- classifyIO x'
        return $ mustBe $ C_Sequence (FJust (Classified cx x'))

classifyTree :: Tree a -> ExceptT Closure IO (Classifier (Tree a))
classifyTree x =
    case x of
      Tree.Node x' _ -> do
        cx <- classifyIO x'
        return $ mustBe $ C_Tree (Classified cx x')

classifyTuple ::
     (SListI xs, IsValidSize (Length xs))
  => NP (K Box) xs
  -> ExceptT Closure IO (Classifier (WrappedTuple xs))
classifyTuple ptrs = do
    cs <- hsequence' (hmap aux ptrs)
    return $ C_Tuple (Classifiers cs)
  where
    aux :: K Box a -> (ExceptT Closure IO :.: Classified) a
    aux (K (Box x)) = Comp $ do
        c <- classifyIO (unsafeCoerce x)
        return $ Classified c (unsafeCoerce x)

{-------------------------------------------------------------------------------
  Recognizing tuples
-------------------------------------------------------------------------------}

isTuple :: String -> Maybe (Some ValidSize)
isTuple typ = do
    (a, xs, z) <- dropEnds typ
    guard $ a == '(' && all (== ',') xs && z == ')'
    toValidSize (length xs + 1)

{-------------------------------------------------------------------------------
  Classified values
-------------------------------------------------------------------------------}

classified :: a -> Either Closure (Classified a)
classified x = (\cx -> Classified cx x) <$> classify x

{-------------------------------------------------------------------------------
  Classify constructor arguments
-------------------------------------------------------------------------------}

-- | Classify the arguments to the constructor
--
-- We only look at pointers and ignore any @UNPACK@ed data. Arguments we cannot
-- classify (like unlifted arguments) will be ignored.
fromUserDefined :: forall c.
     (HasCallStack, KnownConstr c)
  => UserDefined c -> [Some Classified]
fromUserDefined = \(UserDefined x) -> unsafePerformIO $ go x
  where
    go :: x -> IO [Some Classified]
    go x = do
        closure <- getBoxedClosureData (asBox x)
        case closure of
          ConstrClosure {pkg, modl, name, ptrArgs} -> do
            let expected, actual :: Constr String
                expected = knownConstr (sing @_ @c)
                actual   = Constr pkg modl name
            if expected == actual then do
              goArgs [] ptrArgs
            else do
--              tree <- showClosureTree 5 x
              error $ unlines [
                  "elimUserDefined: unexpected constructor"
                , "  closure:  " ++ show closure
                , "  expected: " ++ show expected
                , "  actual:   " ++ show actual
--                , "** TREE **"
--                , tree
--                , "** END OF TREE **"
                ]
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
    case classified x of
      Right classifier -> showClassifiedValue 0 classifier ""
      Left  closure    -> show closure

deriving instance Show (Classifier a)
deriving instance Show (MaybeF     Classified a)
deriving instance Show (EitherF    Classified a b)
deriving instance Show (MaybePairF Classified a b)
deriving instance Show (Some Classified)

instance Show (Classified a) where
  showsPrec p (Classified c x) = showParen (p >= 11) $
      case canShowClassified c of
        Dict ->
            showString "Classified "
          . showsPrec 11 c
          . showString " "
          . showsPrec 11 x

instance SListI xs => Show (Classifiers xs) where
  show (Classifiers xs) = go (hpure Dict)
    where
      go :: NP (Dict (Compose Show Classified)) xs -> String
      go dicts =
          case all_NP dicts of
            Dict -> "(" ++ show xs ++ ")"

-- | Show the classified value (without the classifier)
showClassifiedValue :: Int -> Classified a -> ShowS
showClassifiedValue p (Classified c x) =
    case canShowClassified c of
      Dict -> showsPrec p x

canShowClassified :: Classifier a -> Dict Show a
canShowClassified = go
  where
    go :: Classifier a -> Dict Show a

    --
    -- Simple cases
    --

    -- Primitive types
    go C_Bool     = Dict
    go C_Char     = Dict
    go C_Double   = Dict
    go C_Float    = Dict
    go C_Int      = Dict
    go C_Int16    = Dict
    go C_Int8     = Dict
    go C_Int32    = Dict
    go C_Int64    = Dict
    go C_Integer  = Dict
    go C_Ordering = Dict
    go C_Unit     = Dict
    go C_Word     = Dict
    go C_Word8    = Dict
    go C_Word16   = Dict
    go C_Word32   = Dict
    go C_Word64   = Dict

    -- String types
    go C_String      = Dict
    go C_BS_Strict   = Dict
    go C_BS_Lazy     = Dict
    go C_BS_Short    = Dict
    go C_Text_Strict = Dict
    go C_Text_Lazy   = Dict

    -- Aeson
    go C_Value = Dict

    -- Reference cells
    go C_STRef = Dict
    go C_TVar  = Dict
    go C_MVar  = Dict

    -- Functions
    go C_Fun = Dict

    -- User-defined
    go (C_Custom SConstr) = Dict

    --
    -- Compound
    --

    go (C_Maybe    c) = goMaybeF     c
    go (C_Either   c) = goEitherF    c
    go (C_List     c) = goMaybeF     c
    go (C_Ratio    c) = goF          c
    go (C_Set      c) = goMaybeF     c
    go (C_Map      c) = goMaybePairF c
    go  C_IntSet      = Dict
    go (C_IntMap   c) = goMaybeF     c
    go (C_Sequence c) = goMaybeF     c
    go (C_Tree     c) = goF          c

    go (C_Tuple (Classifiers cs)) =
        case all_NP (hmap (canShowClassified . classifiedType) cs) of
          Dict -> Dict

    goMaybeF :: forall f a.
         (forall x. Show x => Show (f x))
      => MaybeF Classified a -> Dict Show (f a)
    goMaybeF FNothing  = Dict
    goMaybeF (FJust c) = case go (classifiedType c) of
                           Dict -> Dict

    goEitherF :: forall f a b.
         (forall x y. (Show x, Show y) => Show (f x y))
      => EitherF Classified a b -> Dict Show (f a b)
    goEitherF (FLeft  c) = case go (classifiedType c) of
                             Dict -> Dict
    goEitherF (FRight c) = case go (classifiedType c) of
                             Dict -> Dict

    goF :: forall f a.
         (forall x. Show x => Show (f x))
      => Classified a -> Dict Show (f a )
    goF c = case go (classifiedType c) of
              Dict -> Dict

    goMaybePairF :: forall f a b.
         (forall x y. (Show x, Show y) => Show (f x y))
      => MaybePairF Classified a b -> Dict Show (f a b)
    goMaybePairF FNothingPair     = Dict
    goMaybePairF (FJustPair c c') = case ( go (classifiedType c)
                                         , go (classifiedType c')
                                         ) of
                                      (Dict, Dict) -> Dict

instance KnownConstr c => Show (UserDefined c) where
  showsPrec p x =
      case fromUserDefined x of
        [] -> showString constrName
        xs -> showParen (p >= 11)
            . (showString constrName .)
            . foldl (.) id
            . map (\(Some x') -> showString " " . showClassifiedValue 11 x')
            $ xs
    where
      Constr{constrName} = knownConstr (sing @_ @c)
