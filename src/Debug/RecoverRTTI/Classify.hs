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

      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "True"}  -> mustBe C_Bool
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "False"} -> mustBe C_Bool
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "C#"}    -> mustBe C_Char
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "D#"}    -> mustBe C_Double
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "F#"}    -> mustBe C_Float
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "I#"}    -> mustBe C_Int
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "LT"}    -> mustBe C_Ordering
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "GT"}    -> mustBe C_Ordering
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "EQ"}    -> mustBe C_Ordering
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Tuple", name = "()"}    -> mustBe C_Unit
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "W#"}    -> mustBe C_Word

      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I8#"}  -> mustBe C_Int8
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I16#"} -> mustBe C_Int16
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I32#"} -> mustBe C_Int32
      ConstrClosure {pkg = "base", modl = "GHC.Int", name = "I64#"} -> mustBe C_Int64

      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W8#"}  -> mustBe C_Word8
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W16#"} -> mustBe C_Word16
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W32#"} -> mustBe C_Word32
      ConstrClosure {pkg = "base", modl = "GHC.Word", name = "W64#"} -> mustBe C_Word64

      --
      -- String types
      --

      ConstrClosure {pkg, modl = "Data.ByteString.Internal",       name = "PS"}    | "bytestring" `isPrefixOf` pkg -> mustBe C_BS_Strict
      ConstrClosure {pkg, modl = "Data.ByteString.Lazy.Internal",  name = "Empty"} | "bytestring" `isPrefixOf` pkg -> mustBe C_BS_Lazy
      ConstrClosure {pkg, modl = "Data.ByteString.Lazy.Internal",  name = "Chunk"} | "bytestring" `isPrefixOf` pkg -> mustBe C_BS_Lazy
      ConstrClosure {pkg, modl = "Data.ByteString.Short.Internal", name = "SBS"}   | "bytestring" `isPrefixOf` pkg -> mustBe C_BS_Short

      ConstrClosure {pkg, modl = "Data.Text.Internal",      name = "Text"}  | "text" `isPrefixOf` pkg -> mustBe C_Text_Strict
      ConstrClosure {pkg, modl = "Data.Text.Internal.Lazy", name = "Chunk"} | "text" `isPrefixOf` pkg -> mustBe C_Text_Lazy
      ConstrClosure {pkg, modl = "Data.Text.Internal.Lazy", name = "Empty"} | "text" `isPrefixOf` pkg -> mustBe C_Text_Lazy

      --
      -- Compound (ghc-prim)
      --

      -- Lists (this includes the 'String' case)
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = "[]", ptrArgs = []} ->
        mustBe $ C_List Empty
      ConstrClosure {pkg = "ghc-prim", modl = "GHC.Types", name = ":", ptrArgs = [Box x', _xs]} -> do
        c <- classifyIO x'
        case c of
          C_Char     -> mustBe $ C_String
          _otherwise -> mustBe $ C_List (NonEmpty (Classified c x'))

      --
      -- Reference cells
      --

      ConstrClosure {pkg = "base", modl = "GHC.STRef",     name = "STRef"} -> mustBe C_STRef
      ConstrClosure {pkg = "base", modl = "GHC.MVar",      name = "MVar"}  -> mustBe C_MVar
      ConstrClosure {pkg = "base", modl = "GHC.Conc.Sync", name = "TVar"}  -> mustBe C_TVar

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
