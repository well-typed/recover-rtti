{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

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
    -- * Values of an unknown type (failed classification)
  , Unknown(..)
  ) where

import Data.Int
import Data.Kind
import Data.SOP
import Data.Void
import Data.Word
import GHC.Exts
import GHC.Exts.Heap
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI.Constr
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

  -- Compound

  C_List :: MaybeEmpty Classified a -> Classifier [a]

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
      -- Thunk

      ThunkClosure {} ->
        x `seq` classifyIO x

      -- Black holes
      --
      -- For background on black holes, see "Implementing Lazy Functional
      -- Languages on Stock Hardware: The Spineless Tagless G-machine", Simon
      -- Peyton Jones, Journal of Functional Programming, July 1992, section
      -- 9.3.3 "Black holes".

      BlackholeClosure _ (Box x') ->
        unsafeCoerce <$> classifyIO x'

      -- Primitive (ghc-prim)

      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "True"} ->
        mustBe C_Bool
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "False"} ->
        mustBe C_Bool
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "C#"} ->
        mustBe C_Char
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "D#"} ->
        mustBe C_Double
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "F#"} ->
        mustBe C_Float
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "I#"} ->
        mustBe C_Int
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I8#"} ->
        mustBe C_Int8
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I16#"} ->
        mustBe C_Int16
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I32#"} ->
        mustBe C_Int32
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I64#"} ->
        mustBe C_Int64
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "LT"} ->
        mustBe C_Ordering
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "GT"} ->
        mustBe C_Ordering
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "EQ"} ->
        mustBe C_Ordering
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Tuple", name = "()"} ->
        mustBe C_Unit
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "W#"} ->
        mustBe C_Word
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W8#"} ->
        mustBe C_Word8
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W16#"} ->
        mustBe C_Word16
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W32#"} ->
        mustBe C_Word32
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W64#"} ->
        mustBe C_Word64

      -- Compound (ghc-prim)
      --
      -- NOTE: We use 'Void' for the type of elements of empty containers.

      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "[]", ptrArgs = []} -> do
        mustBe   $ C_List Empty
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = ":", ptrArgs = [Box x', _xs]} -> do
        c <- classifyIO x'
        mustBe $ C_List (NonEmpty (Classified c x'))

      -- User defined

      ConstrClosure {pkg, modl, name} ->
        elimKnownConstr (Constr pkg modl name) $ \p ->
        mustBe $ C_Custom p

      _otherwise ->
        mustBe $ C_Unknown
  where
    mustBe :: Classifier b -> IO (Classifier a)
    mustBe = return . unsafeCoerce

classify :: a -> Classifier a
classify = unsafePerformIO . classifyIO

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
          BlackholeClosure _ (Box x') ->
            go x'
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
