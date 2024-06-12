{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

-- | Modules we recognize types from
module Debug.RecoverRTTI.Modules (
    KnownPkg(..)
  , KnownModule(..)
  , IsKnownPkg(..)
    -- * Matching
  , inKnownModule
  , inKnownModuleNested
  ) where

import Control.Monad
import Data.List (isPrefixOf)

import Debug.RecoverRTTI.FlatClosure

{-------------------------------------------------------------------------------
  Packages
-------------------------------------------------------------------------------}

data KnownPkg =
    PkgGhcPrim
#if MIN_VERSION_base(4,20,0)
  | PkgGhcInternal
#endif
  | PkgBase
#if !MIN_VERSION_base(4,17,0)
  | PkgDataArrayByte
#endif
  | PkgByteString
  | PkgText
  | PkgIntegerWiredIn
  | PkgGhcBignum
  | PkgContainers
  | PkgAeson
  | PkgUnorderedContainers
  | PkgVector
  | PkgPrimitive

data family KnownModule (pkg :: KnownPkg)

{-------------------------------------------------------------------------------
  Singleton instance for KnownPkg
-------------------------------------------------------------------------------}

data SPkg (pkg :: KnownPkg) where
  SGhcPrim             :: SPkg 'PkgGhcPrim
#if MIN_VERSION_base(4,20,0)
  SGhcInternal         :: SPkg 'PkgGhcInternal
#endif
  SBase                :: SPkg 'PkgBase
#if !MIN_VERSION_base(4,17,0)
  SDataArrayByte       :: SPkg 'PkgDataArrayByte
#endif
  SByteString          :: SPkg 'PkgByteString
  SText                :: SPkg 'PkgText
  SIntegerWiredIn      :: SPkg 'PkgIntegerWiredIn
  SGhcBignum           :: SPkg 'PkgGhcBignum
  SContainers          :: SPkg 'PkgContainers
  SAeson               :: SPkg 'PkgAeson
  SUnorderedContainers :: SPkg 'PkgUnorderedContainers
  SVector              :: SPkg 'PkgVector
  SPrimitive           :: SPkg 'PkgPrimitive

class IsKnownPkg pkg where
  singPkg :: SPkg pkg

instance IsKnownPkg 'PkgGhcPrim             where singPkg = SGhcPrim
#if MIN_VERSION_base(4,20,0)
instance IsKnownPkg 'PkgGhcInternal         where singPkg = SGhcInternal
#endif
instance IsKnownPkg 'PkgBase                where singPkg = SBase
#if !MIN_VERSION_base(4,17,0)
instance IsKnownPkg 'PkgDataArrayByte       where singPkg = SDataArrayByte
#endif
instance IsKnownPkg 'PkgByteString          where singPkg = SByteString
instance IsKnownPkg 'PkgText                where singPkg = SText
instance IsKnownPkg 'PkgIntegerWiredIn      where singPkg = SIntegerWiredIn
instance IsKnownPkg 'PkgGhcBignum           where singPkg = SGhcBignum
instance IsKnownPkg 'PkgContainers          where singPkg = SContainers
instance IsKnownPkg 'PkgAeson               where singPkg = SAeson
instance IsKnownPkg 'PkgUnorderedContainers where singPkg = SUnorderedContainers
instance IsKnownPkg 'PkgVector              where singPkg = SVector
instance IsKnownPkg 'PkgPrimitive           where singPkg = SPrimitive

{-------------------------------------------------------------------------------
  Modules in @ghc-prim@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgGhcPrim =
    GhcTypes
  | GhcTuple

{-------------------------------------------------------------------------------
  Modules in @ghc-internal@ (ghc 9.10 and up)
-------------------------------------------------------------------------------}

#if MIN_VERSION_base(4,20,0)
data instance KnownModule 'PkgGhcInternal =
    GhcInt
  | GhcWord
  | GhcSTRef
  | GhcMVar
  | GhcConcSync
  | GhcMaybe
  | GhcReal
  | DataEither
#endif

{-------------------------------------------------------------------------------
  Modules in @base@
-------------------------------------------------------------------------------}


#if MIN_VERSION_base(4,20,0)

data instance KnownModule 'PkgBase =
    DataArrayByte

#else

data instance KnownModule 'PkgBase =
    GhcInt
  | GhcWord
  | GhcSTRef
  | GhcMVar
  | GhcConcSync
  | GhcMaybe
  | GhcReal
  | DataEither

#if MIN_VERSION_base(4,17,0)
  | DataArrayByte
#else
data instance KnownModule 'PkgDataArrayByte =
    DataArrayByte
#endif

#endif

{-------------------------------------------------------------------------------
  Modules in @bytestring@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgByteString =
    DataByteStringInternal
  | DataByteStringLazyInternal
  | DataByteStringShortInternal

{-------------------------------------------------------------------------------
  Modules in @text@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgText =
    DataTextInternal
  | DataTextInternalLazy

{-------------------------------------------------------------------------------
  Modules in @integer-wired-in@ (this is a virtual package)
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgIntegerWiredIn =
    GhcIntegerType

{-------------------------------------------------------------------------------
  Modules in @ghc-bignum@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgGhcBignum =
    GhcNumInteger

{-------------------------------------------------------------------------------
  Modules in @containers@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgContainers =
    DataSetInternal
  | DataMapInternal
  | DataIntSetInternal
  | DataIntMapInternal
  | DataSequenceInternal
  | DataTree

{-------------------------------------------------------------------------------
  Modules in @aeson@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgAeson =
    DataAesonTypesInternal

{-------------------------------------------------------------------------------
  Modules in @unordered-containers@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgUnorderedContainers =
    DataHashMapInternal
  | DataHashMapInternalArray

{-------------------------------------------------------------------------------
  Modules in @vector@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgVector =
    DataVector
  | DataVectorStorable
  | DataVectorStorableMutable
  | DataVectorPrimitive
  | DataVectorPrimitiveMutable

{-------------------------------------------------------------------------------
  Modules in @primitive@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgPrimitive =
    DataPrimitiveArray
  | DataPrimitiveByteArray

{-------------------------------------------------------------------------------
  Matching
-------------------------------------------------------------------------------}

-- | Check if the given closure is from a known package/module
inKnownModule :: IsKnownPkg pkg
  => KnownModule pkg
  -> FlatClosure -> Maybe String
inKnownModule modl = fmap fst . inKnownModuleNested modl

-- | Generalization of 'inKnownModule' that additionally returns nested pointers
inKnownModuleNested :: IsKnownPkg pkg
  => KnownModule pkg
  -> FlatClosure -> Maybe (String, [Box])
inKnownModuleNested = go singPkg
  where
    -- We ignore the package version: we assume that we are linked against only
    -- a single version of each package, and that those versions are statically
    -- known (that is, we can use CPP where necessary).
    go :: SPkg pkg -> KnownModule pkg -> FlatClosure -> Maybe (String, [Box])
    go knownPkg knownModl ConstrClosure{pkg, modl, name, ptrArgs} = do
        guard (stripVowels (namePkg knownPkg) `isPrefixOf` stripVowels pkg)
        guard (modl == nameModl knownPkg knownModl)
        return (name, ptrArgs)
    go _ _ _otherClosure = Nothing

    namePkg :: SPkg pkg -> String
    namePkg SGhcPrim             = "ghc-prim"
#if MIN_VERSION_base(4,20,0)
    namePkg SGhcInternal         = "ghc-internal"
#endif
    namePkg SBase                = "base"
#if !MIN_VERSION_base(4,17,0)
    namePkg SDataArrayByte       = "data-array-byte"
#endif
    namePkg SByteString          = "bytestring"
    namePkg SText                = "text"
    namePkg SIntegerWiredIn      = "integer-wired-in"
    namePkg SGhcBignum           = "ghc-bignum"
    namePkg SContainers          = "containers"
    namePkg SAeson               = "aeson"
    namePkg SUnorderedContainers = "unordered-containers"
    namePkg SVector              = "vector"
    namePkg SPrimitive           = "primitive"

    nameModl :: SPkg pkg -> KnownModule pkg -> String
    nameModl = \case
        SGhcPrim -> \case
          GhcTypes -> "GHC.Types"

#if MIN_VERSION_base(4,20,0)
          GhcTuple -> "GHC.Tuple"
#elif MIN_VERSION_ghc_prim(0,10,0)
          GhcTuple -> "GHC.Tuple.Prim"
#else
          GhcTuple -> "GHC.Tuple"
#endif

#if MIN_VERSION_base(4,20,0)
        SGhcInternal -> \case
          GhcInt      -> "GHC.Internal.Int"
          GhcWord     -> "GHC.Internal.Word"
          GhcSTRef    -> "GHC.Internal.STRef"
          GhcMVar     -> "GHC.Internal.MVar"
          GhcConcSync -> "GHC.Internal.Conc.Sync"
          GhcMaybe    -> "GHC.Internal.Maybe"
          GhcReal     -> "GHC.Internal.Real"
          DataEither  -> "GHC.Internal.Data.Either"
#endif

        SBase -> \case
#if !MIN_VERSION_base(4,20,0)
          GhcInt        -> "GHC.Int"
          GhcWord       -> "GHC.Word"
          GhcSTRef      -> "GHC.STRef"
          GhcMVar       -> "GHC.MVar"
          GhcConcSync   -> "GHC.Conc.Sync"
          GhcMaybe      -> "GHC.Maybe"
          GhcReal       -> "GHC.Real"
          DataEither    -> "Data.Either"
#endif
#if MIN_VERSION_base(4,17,0)
          DataArrayByte -> "Data.Array.Byte"
#else
        SDataArrayByte -> \case
          DataArrayByte -> "Data.Array.Byte"
#endif

        SByteString -> \case
#if MIN_VERSION_bytestring(0,11,4)
          DataByteStringInternal      -> "Data.ByteString.Internal.Type"
#else
          DataByteStringInternal      -> "Data.ByteString.Internal"
#endif
          DataByteStringLazyInternal  -> "Data.ByteString.Lazy.Internal"
          DataByteStringShortInternal -> "Data.ByteString.Short.Internal"

        SText -> \case
          DataTextInternal     -> "Data.Text.Internal"
          DataTextInternalLazy -> "Data.Text.Internal.Lazy"

        SIntegerWiredIn -> \case
          GhcIntegerType -> "GHC.Integer.Type"

        SGhcBignum -> \case
          GhcNumInteger -> "GHC.Num.Integer"

        SContainers -> \case
          DataSetInternal      -> "Data.Set.Internal"
          DataMapInternal      -> "Data.Map.Internal"
          DataIntSetInternal   -> "Data.IntSet.Internal"
          DataIntMapInternal   -> "Data.IntMap.Internal"
          DataSequenceInternal -> "Data.Sequence.Internal"
          DataTree             -> "Data.Tree"

        SAeson -> \case
          DataAesonTypesInternal -> "Data.Aeson.Types.Internal"

        SUnorderedContainers -> \case
          DataHashMapInternal      -> "Data.HashMap.Internal"
          DataHashMapInternalArray -> "Data.HashMap.Internal.Array"

        SVector -> \case
          DataVector                 -> "Data.Vector"
          DataVectorStorable         -> "Data.Vector.Storable"
          DataVectorStorableMutable  -> "Data.Vector.Storable.Mutable"
          DataVectorPrimitive        -> "Data.Vector.Primitive"
          DataVectorPrimitiveMutable -> "Data.Vector.Primitive.Mutable"

        SPrimitive -> \case
          DataPrimitiveArray     -> "Data.Primitive.Array"
          DataPrimitiveByteArray -> "Data.Primitive.ByteArray"

    -- On OSX, cabal strips vowels from package IDs in order to work around
    -- limitations around path lengths
    -- <https://github.com/haskell/cabal/blob/3f397c0c661facd0be9c5c67ad26f66a87725472/cabal-install/src/Distribution/Client/PackageHash.hs#L125-L157>
    stripVowels :: String -> String
    stripVowels = filter (`notElem` "aeoiu")
