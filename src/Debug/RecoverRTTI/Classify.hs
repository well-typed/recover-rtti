{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Debug.RecoverRTTI.Classify (
    -- * Classification
    classify
  , classified
  , fromUserDefined
    -- * Showing values
  , showAnything
  , canShowClassified
  ) where

import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.SOP
import Data.SOP.Dict
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.FlatClosure
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.UserDefined
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

classifyIO :: a -> IO (Classifier a)
classifyIO x = do
    closure <- getBoxedClosureData (asBox x)
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
      -- Compound (ghc-prim)
      --

      -- Lists (this includes the 'String' case)
      (inKnownModuleNested GhcTypes -> Just ("[]", [])) ->
        return $ mustBe $ C_List Empty
      (inKnownModuleNested GhcTypes -> Just (":", [Box x', _xs])) -> do
        c <- classifyIO x'
        return $ case c of
          C_Char     -> mustBe $ C_String
          _otherwise -> mustBe $ C_List (NonEmpty (Classified c x'))

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
      PAPClosure {} -> return $ mustBe C_Fun

      --
      -- User defined
      --

      ConstrClosure {pkg, modl, name} ->
        elimKnownConstr (Constr pkg modl name) $ \p ->
        return $ mustBe (C_Custom p)

      --
      -- Unsupported
      --

      OtherClosure _ ->
        return $ mustBe C_Unknown

classifyTuple ::
     (SListI xs, IsValidSize (Length xs))
  => NP (K Box) xs
  -> IO (Classifier (WrappedTuple xs))
classifyTuple ptrs = do
    cs <- hsequence' (hmap aux ptrs)
    return $ C_Tuple (Classifiers cs)
  where
    aux :: K Box a -> (IO :.: Classified) a
    aux (K (Box x)) = Comp $ do
        c <- classifyIO (unsafeCoerce x)
        return $ Classified c (unsafeCoerce x)

mustBe :: Classifier b -> Classifier a
mustBe = unsafeCoerce

classify :: a -> Classifier a
classify = unsafePerformIO . classifyIO

{-------------------------------------------------------------------------------
  Recognizing tuples
-------------------------------------------------------------------------------}

isTuple :: String -> Maybe (Some ValidSize)
isTuple typ = do
    (a, xs, z) <- dropEnds typ
    guard $ a == '(' && all (== ',') xs && z == ')'
    toValidSize (length xs + 1)

{-------------------------------------------------------------------------------
  Modules we classify types from
-------------------------------------------------------------------------------}

data family KnownModule (pkg :: KnownPkg)

data KnownPkg =
    PkgGhcPrim
  | PkgBase
  | PkgByteString
  | PkgText

data instance Sing (pkg :: KnownPkg) where
  SGhcPrim    :: Sing 'PkgGhcPrim
  SBase       :: Sing 'PkgBase
  SByteString :: Sing 'PkgByteString
  SText       :: Sing 'PkgText

instance SingI 'PkgGhcPrim    where sing = SGhcPrim
instance SingI 'PkgBase       where sing = SBase
instance SingI 'PkgByteString where sing = SByteString
instance SingI 'PkgText       where sing = SText

data instance KnownModule 'PkgGhcPrim =
    GhcTypes
  | GhcTuple

data instance KnownModule 'PkgBase =
    GhcInt
  | GhcWord
  | GhcSTRef
  | GhcMVar
  | GhcConcSync

data instance KnownModule 'PkgByteString =
    DataByteStringInternal
  | DataByteStringLazyInternal
  | DataByteStringShortInternal

data instance KnownModule 'PkgText =
    DataTextInternal
  | DataTextInternalLazy

-- | Check if the given closure is from a known package/module
inKnownModule :: SingI pkg
  => KnownModule pkg
  -> FlatClosure -> Maybe String
inKnownModule modl = fmap fst . inKnownModuleNested modl

-- | Generalization of 'inKnownModule' that additionally returns nested pointers
inKnownModuleNested :: SingI pkg
  => KnownModule pkg
  -> FlatClosure -> Maybe (String, [Box])
inKnownModuleNested = go sing
  where
    go :: Sing pkg -> KnownModule pkg -> FlatClosure -> Maybe (String, [Box])
    go knownPkg knownModl ConstrClosure{pkg, modl, name, ptrArgs} = do
        guard (namePkg knownPkg `isPrefixOf` pkg) -- ignore the version number
        guard (modl == nameModl knownPkg knownModl)
        return (name, ptrArgs)
    go _ _ _otherClosure = Nothing

    namePkg :: Sing (pkg :: KnownPkg) -> String
    namePkg SGhcPrim    = "ghc-prim"
    namePkg SBase       = "base"
    namePkg SByteString = "bytestring"
    namePkg SText       = "text"

    nameModl :: Sing (pkg :: KnownPkg) -> KnownModule pkg -> String
    nameModl SGhcPrim    GhcTypes                    = "GHC.Types"
    nameModl SGhcPrim    GhcTuple                    = "GHC.Tuple"
    nameModl SBase       GhcInt                      = "GHC.Int"
    nameModl SBase       GhcWord                     = "GHC.Word"
    nameModl SBase       GhcSTRef                    = "GHC.STRef"
    nameModl SBase       GhcMVar                     = "GHC.MVar"
    nameModl SBase       GhcConcSync                 = "GHC.Conc.Sync"
    nameModl SByteString DataByteStringInternal      = "Data.ByteString.Internal"
    nameModl SByteString DataByteStringLazyInternal  = "Data.ByteString.Lazy.Internal"
    nameModl SByteString DataByteStringShortInternal = "Data.ByteString.Short.Internal"
    nameModl SText       DataTextInternal            = "Data.Text.Internal"
    nameModl SText       DataTextInternalLazy        = "Data.Text.Internal.Lazy"

{-------------------------------------------------------------------------------
  Classified values
-------------------------------------------------------------------------------}

classified :: a -> Classified a
classified x = Classified (classify x) x

{-------------------------------------------------------------------------------
  Classify constructor arguments
-------------------------------------------------------------------------------}

-- | Classify the arguments to the constructor
--
-- We only look at pointers and ignore any @UNPACK@ed data.
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
            if expected == actual then
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
        c <- classifyIO b
        goArgs (Some (Classified c (unsafeCoerce b)) : acc) bs

{-------------------------------------------------------------------------------
  Show

  Showing values is mutually recursive with classification: when we show a
  value classified as @UserDefined@, we recursively classify the nested values
  /when/ we show the value.
-------------------------------------------------------------------------------}

-- | Show any value
showAnything :: forall a. a -> String
showAnything x = showClassifiedValue 0 (classified x) ""

deriving instance Show (Classifier a)
deriving instance Show (MaybeEmpty Classified a)
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
canShowClassified = \case
    -- Primitive types

    C_Bool     -> Dict
    C_Char     -> Dict
    C_Double   -> Dict
    C_Float    -> Dict
    C_Int      -> Dict
    C_Int16    -> Dict
    C_Int8     -> Dict
    C_Int32    -> Dict
    C_Int64    -> Dict
    C_Ordering -> Dict
    C_Unit     -> Dict
    C_Word     -> Dict
    C_Word8    -> Dict
    C_Word16   -> Dict
    C_Word32   -> Dict
    C_Word64   -> Dict

    -- String types

    C_String      -> Dict
    C_BS_Strict   -> Dict
    C_BS_Lazy     -> Dict
    C_BS_Short    -> Dict
    C_Text_Strict -> Dict
    C_Text_Lazy   -> Dict

    -- Compound

    C_List Empty -> Dict
    C_List (NonEmpty c') ->
        case canShowClassified (classifiedType c') of
          Dict -> Dict

    C_Tuple (Classifiers cs) ->
        case all_NP (hmap (canShowClassified . classifiedType) cs) of
          Dict -> Dict

    -- Reference cells

    C_STRef -> Dict
    C_TVar  -> Dict
    C_MVar  -> Dict

    -- Functions

    C_Fun -> Dict

    -- User-defined

    C_Custom SConstr -> Dict

    -- Classification failed

    C_Unknown -> Dict

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
