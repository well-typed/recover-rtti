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
  , Classifiers(..)
  , classify
    -- * Bundle value with its classifier
  , Classified(..)
  , classified
    -- * User-defined types
  , UserDefined -- opaque
  , fromUserDefined
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP
import Data.SOP.Dict
import Data.Void
import GHC.Exts
import GHC.Exts.Heap
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI.ConstrInfo

{-------------------------------------------------------------------------------
  Classifier
-------------------------------------------------------------------------------}

data Classifier (a :: Type) :: Type where
  -- Primitive types

  C_Void   :: Classifier Void
  C_Unit   :: Classifier ()
  C_Bool   :: Classifier Bool
  C_Int    :: Classifier Int
  C_Char   :: Classifier Char
  C_Double :: Classifier Double

  -- Compound

  C_List   :: Classifier a -> Classifier [a]

  -- User-defined

  C_Custom :: ProxyKnownConstr n -> Classifier (UserDefined n)

newtype Classifiers xs = Classifiers (NP Classifier xs)

instance SListI xs => Show (Classifiers xs) where
  show (Classifiers xs) = go (hpure Dict)
    where
      go :: NP (Dict (Compose Show Classifier)) xs -> String
      go dicts =
          case all_NP dicts of
            Dict -> "(" ++ show xs ++ ")"

deriving instance Show (Classifier a)

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

      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Tuple", name = "()"} ->
        mustBe C_Unit
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "True"} ->
        mustBe C_Bool
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "False"} ->
        mustBe C_Bool
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "I#"} ->
        mustBe C_Int
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "C#"} ->
        mustBe C_Char
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "D#"} ->
        mustBe C_Double

      -- Compound (ghc-prim)
      --
      -- NOTE: We use 'Void' for the type of elements of empty containers.

      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "[]", ptrArgs = []} -> do
        mustBe $ C_List C_Void
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = ":", ptrArgs = [Box x', _xs]} -> do
        c <- classifyIO x'
        mustBe $ C_List c

      -- User defined

      ConstrClosure {pkg, modl, name} ->
        elimKnownConstr (ConstrInfo pkg modl name) $ \p ->
        mustBe $ C_Custom p

      _otherwise ->
        error $ "Unexpected closure: " ++ show closure
  where
    mustBe :: Classifier b -> IO (Classifier a)
    mustBe = return . unsafeCoerce

classify :: a -> Classifier a
classify = unsafePerformIO . classifyIO

{-------------------------------------------------------------------------------
  Classified values
-------------------------------------------------------------------------------}

-- | A value along with its classifier
--
-- This is similar to 'Data.Dynamic.Dynamic'.
data Classified where
    Classified :: Classifier a -> a -> Classified

classified :: a -> Classified
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
newtype UserDefined (c :: ConstrInfo Symbol) = UserDefined Any

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromUserDefined :: forall c. KnownConstr c => UserDefined c -> [Classified]
fromUserDefined = \(UserDefined x) -> unsafePerformIO $ go x
  where
    go :: x -> IO [Classified]
    go x = do
        closure <- getBoxedClosureData (asBox x)
        case closure of
          BlackholeClosure _ (Box x') ->
            go x'
          ConstrClosure {pkg, modl, name, ptrArgs} ->
            let expected, actual :: ConstrInfo String
                expected = knownConstr (Proxy @c)
                actual   = ConstrInfo pkg modl name
            in if expected == actual
                 then goArgs [] ptrArgs
                 else error $ "elimUserDefined: unexpected constructor: "
                           ++ show closure
          _otherwise ->
            error $ "elimUserDefined: unexpected closure: "
                 ++ show closure

    goArgs :: [Classified] -> [Box] -> IO [Classified]
    goArgs acc []         = return (reverse acc)
    goArgs acc (Box b:bs) = do
        c <- classifyIO b
        goArgs (Classified c (unsafeCoerce b) : acc) bs
