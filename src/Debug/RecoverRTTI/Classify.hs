{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Debug.RecoverRTTI.Classify (
    -- * Recover RTTI
    Classifier(..)
  , MaybeEmpty(..)
  , Classifiers(..)
  , classify
    -- * Bundle value with its classifier
  , Classified(..)
  , classified
    -- * User-defined types
  , UserDefined -- opaque
  , unsafeCoerceUserDefined
  , fromUserDefined
    -- * Reference cells
  , SomeSTRef(..)
  , SomeMVar(..)
  , SomeTVar(..)
    -- * Functions
  , SomeFun(..)
    -- * Values of an unknown type (failed classification)
  , Unknown(..)
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Control.Monad (guard)
import Data.Int
import Data.Kind
import Data.List (isPrefixOf)
import Data.SOP
import Data.STRef (STRef)
import Data.Void
import Data.Word
import GHC.Exts
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.ByteString       as BS.Strict
import qualified Data.ByteString.Lazy  as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import qualified Data.Text             as Text.Strict
import qualified Data.Text.Lazy        as Text.Lazy

import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.FlatClosure
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Classifier
-------------------------------------------------------------------------------}

data Classifier (a :: Type) :: Type where
  -- Primitive types

  C_Bool     :: Classifier Bool
  C_Char     :: Classifier Char
  C_Double   :: Classifier Double
  C_Float    :: Classifier Float
  C_Int      :: Classifier Int
  C_Int16    :: Classifier Int16
  C_Int8     :: Classifier Int8
  C_Int32    :: Classifier Int32
  C_Int64    :: Classifier Int64
  C_Ordering :: Classifier Ordering
  C_Unit     :: Classifier ()
  C_Word     :: Classifier Word
  C_Word8    :: Classifier Word8
  C_Word16   :: Classifier Word16
  C_Word32   :: Classifier Word32
  C_Word64   :: Classifier Word64

  -- String types
  --
  -- We list @String@ separately, so that we show them properly (rather than
  -- as a list of characters). Of course, empty strings will be inferred as
  -- empty lists instead.

  C_String      :: Classifier String
  C_BS_Strict   :: Classifier BS.Strict.ByteString
  C_BS_Lazy     :: Classifier BS.Lazy.ByteString
  C_BS_Short    :: Classifier BS.Short.ShortByteString
  C_Text_Strict :: Classifier Text.Strict.Text
  C_Text_Lazy   :: Classifier Text.Lazy.Text

  -- Compound

  C_List :: MaybeEmpty Classified a -> Classifier [a]

  -- Reference cells

  C_STRef :: Classifier SomeSTRef
  C_TVar  :: Classifier SomeTVar
  C_MVar  :: Classifier SomeMVar

  -- Functions

  C_Fun :: Classifier SomeFun

  -- User-defined

  C_Custom :: Sing c -> Classifier (UserDefined c)

  -- Classification failed

  C_Unknown :: Classifier Unknown

data MaybeEmpty f a where
  Empty    :: MaybeEmpty f Void
  NonEmpty :: f a -> MaybeEmpty f a

newtype Classifiers xs = Classifiers (NP Classifier xs)

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

-- | A value along with its classifier
data Classified a = Classified (Classifier a) a

classified :: a -> Classified a
classified x = Classified (classify x) x

{-------------------------------------------------------------------------------
  User-defined types
-------------------------------------------------------------------------------}

-- | User-defined type
--
-- For user-defined types we recover, at the type level, information about the
-- constructor. In /principle/ of course this means that this tells us what
-- the type of this thing is; if
--
-- > data MyType .. = MyConstr .. | ...
--
-- then @coerce :: UserDefined "MyConstr" -> MyType@ should be safe.
--
-- We defer classification of the /arguments/ to the constructor. This is
-- necessary, because if we tried to do this eagerly---recording those types as
-- part of the 'UserDefined' type---we might end up "unwinding" recursive types
-- at the type level; for example, something like
--
-- > data MyList = MyNil | MyCons a (MyList a)
--
-- could then result in something like
--
-- > UserDefined "MyCons" '[ Int, UserDefined "MyCons" '[ Int , ... ] .. ]
--
-- Detecting recursion is undecidable (that's why Haskell uses isorecursive
-- rather than equirecursive types), so instead we defer.
newtype UserDefined (c :: Constr Symbol) = UserDefined Any

{-------------------------------------------------------------------------------
  Reference cells

  We do not try to look inside these variables to figure out the type of their
  elements; the show instance merely shows an address.
-------------------------------------------------------------------------------}

newtype SomeSTRef = SomeSTRef (STRef Any Any)
  deriving (Eq)

newtype SomeMVar = SomeMVar (MVar Any)
  deriving (Eq)

newtype SomeTVar = SomeTVar (TVar Any)
  deriving (Eq)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

-- | Functions
--
-- We do not try to infer the domain or codomain of the function.
newtype SomeFun = SomeFun (Any -> Any)

{-------------------------------------------------------------------------------
  Unknown values
-------------------------------------------------------------------------------}

-- | We classify unknown values as 'Unknown'
newtype Unknown = Unknown Any

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

unsafeCoerceUserDefined :: forall a c. ConstrOf a c => UserDefined c -> a
unsafeCoerceUserDefined = unsafeCoerce
  where
    _ = keepRedundantConstraint (Proxy @(ConstrOf a c))

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
