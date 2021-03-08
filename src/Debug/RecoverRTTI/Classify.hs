{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}

module Debug.RecoverRTTI.Classify (
    classify
  , classified
  , fromUserDefined
  ) where

import Control.Monad (guard)
import Data.List (isPrefixOf)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.FlatClosure
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
      (inKnownModule GhcTypes -> Just "True")  -> mustBe C_Bool
      (inKnownModule GhcTypes -> Just "False") -> mustBe C_Bool
      (inKnownModule GhcTypes -> Just "C#")    -> mustBe C_Char
      (inKnownModule GhcTypes -> Just "D#")    -> mustBe C_Double
      (inKnownModule GhcTypes -> Just "F#")    -> mustBe C_Float
      (inKnownModule GhcTypes -> Just "I#")    -> mustBe C_Int
      (inKnownModule GhcTypes -> Just "LT")    -> mustBe C_Ordering
      (inKnownModule GhcTypes -> Just "GT")    -> mustBe C_Ordering
      (inKnownModule GhcTypes -> Just "EQ")    -> mustBe C_Ordering
      (inKnownModule GhcTypes -> Just "W#")    -> mustBe C_Word

      -- GHC.Tuple
      (inKnownModule GhcTuple -> Just "()") -> mustBe C_Unit

      -- GHC.Int
      (inKnownModule GhcInt -> Just "I8#")  -> mustBe C_Int8
      (inKnownModule GhcInt -> Just "I16#") -> mustBe C_Int16
      (inKnownModule GhcInt -> Just "I32#") -> mustBe C_Int32
      (inKnownModule GhcInt -> Just "I64#") -> mustBe C_Int64

      -- GHC.Word
      (inKnownModule GhcWord -> Just "W8#")  -> mustBe C_Word8
      (inKnownModule GhcWord -> Just "W16#") -> mustBe C_Word16
      (inKnownModule GhcWord -> Just "W32#") -> mustBe C_Word32
      (inKnownModule GhcWord -> Just "W64#") -> mustBe C_Word64

      --
      -- String types
      --

      -- bytestring
      (inKnownModule DataByteStringInternal      -> Just "PS")    -> mustBe C_BS_Strict
      (inKnownModule DataByteStringLazyInternal  -> Just "Empty") -> mustBe C_BS_Lazy
      (inKnownModule DataByteStringLazyInternal  -> Just "Chunk") -> mustBe C_BS_Lazy
      (inKnownModule DataByteStringShortInternal -> Just "SBS")   -> mustBe C_BS_Short

      -- text
      (inKnownModule DataTextInternal     -> Just "Text")  -> mustBe C_Text_Strict
      (inKnownModule DataTextInternalLazy -> Just "Chunk") -> mustBe C_Text_Lazy
      (inKnownModule DataTextInternalLazy -> Just "Empty") -> mustBe C_Text_Lazy

      --
      -- Compound (ghc-prim)
      --

      -- Lists (this includes the 'String' case)
      (inKnownModuleNested GhcTypes -> Just ("[]", [])) ->
        mustBe $ C_List Empty
      (inKnownModuleNested GhcTypes -> Just (":", [Box x', _xs])) -> do
        c <- classifyIO x'
        case c of
          C_Char     -> mustBe $ C_String
          _otherwise -> mustBe $ C_List (NonEmpty (Classified c x'))

      --
      -- Reference cells
      --

      (inKnownModule GhcSTRef    -> Just "STRef") -> mustBe C_STRef
      (inKnownModule GhcMVar     -> Just "MVar")  -> mustBe C_MVar
      (inKnownModule GhcConcSync -> Just "TVar")  -> mustBe C_TVar

      --
      -- Functions
      --

      FunClosure {} -> mustBe C_Fun
      PAPClosure {} -> mustBe C_Fun

      --
      -- User defined
      --

      ConstrClosure {pkg, modl, name} ->
        elimKnownConstr (Constr pkg modl name) $ \p ->
        mustBe $ C_Custom p

      --
      -- Unsupported
      --

      OtherClosure _ ->
        mustBe $ C_Unknown
  where
    mustBe :: Classifier b -> IO (Classifier a)
    mustBe = return . unsafeCoerce

classify :: a -> Classifier a
classify = unsafePerformIO . classifyIO

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
fromUserDefined :: forall c. KnownConstr c => UserDefined c -> [Some Classified]
fromUserDefined = \(UserDefined x) -> unsafePerformIO $ go x
  where
    go :: x -> IO [Some Classified]
    go x = do
        closure <- getBoxedClosureData (asBox x)
        case closure of
          ConstrClosure {pkg, modl, name, ptrArgs} ->
            let expected, actual :: Constr String
                expected = knownConstr (sing @_ @c)
                actual   = Constr pkg modl name
            in if expected == actual
                 then goArgs [] ptrArgs
                 else error $ "elimUserDefined: unexpected constructor: "
                           ++ show closure
          _otherwise ->
            error $ "elimUserDefined: unexpected closure: "
                 ++ show closure

    goArgs :: [Some Classified] -> [Box] -> IO [Some Classified]
    goArgs acc []         = return (reverse acc)
    goArgs acc (Box b:bs) = do
        c <- classifyIO b
        goArgs (Exists (Classified c (unsafeCoerce b)) : acc) bs
