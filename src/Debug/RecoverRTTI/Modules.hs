{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
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
  | PkgBase
  | PkgByteString
  | PkgText
  | PkgIntegerWiredIn
  | PkgGhcBignum
  | PkgContainers
  | PkgAeson
  | PkgUnorderedContainers
  | PkgVector

data family KnownModule (pkg :: KnownPkg)

{-------------------------------------------------------------------------------
  Singleton instance for KnownPkg
-------------------------------------------------------------------------------}

data SPkg (pkg :: KnownPkg) where
  SGhcPrim             :: SPkg 'PkgGhcPrim
  SBase                :: SPkg 'PkgBase
  SByteString          :: SPkg 'PkgByteString
  SText                :: SPkg 'PkgText
  SIntegerWiredIn      :: SPkg 'PkgIntegerWiredIn
  SGhcBignum           :: SPkg 'PkgGhcBignum
  SContainers          :: SPkg 'PkgContainers
  SAeson               :: SPkg 'PkgAeson
  SUnorderedContainers :: SPkg 'PkgUnorderedContainers
  SVector              :: SPkg 'PkgVector

class IsKnownPkg pkg where
  singPkg :: SPkg pkg

instance IsKnownPkg 'PkgGhcPrim             where singPkg = SGhcPrim
instance IsKnownPkg 'PkgBase                where singPkg = SBase
instance IsKnownPkg 'PkgByteString          where singPkg = SByteString
instance IsKnownPkg 'PkgText                where singPkg = SText
instance IsKnownPkg 'PkgIntegerWiredIn      where singPkg = SIntegerWiredIn
instance IsKnownPkg 'PkgGhcBignum           where singPkg = SGhcBignum
instance IsKnownPkg 'PkgContainers          where singPkg = SContainers
instance IsKnownPkg 'PkgAeson               where singPkg = SAeson
instance IsKnownPkg 'PkgUnorderedContainers where singPkg = SUnorderedContainers
instance IsKnownPkg 'PkgVector              where singPkg = SVector

{-------------------------------------------------------------------------------
  Modules in @ghc-pri@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgGhcPrim =
    GhcTypes
  | GhcTuple

{-------------------------------------------------------------------------------
  Modules in @base@
-------------------------------------------------------------------------------}

data instance KnownModule 'PkgBase =
    GhcInt
  | GhcWord
  | GhcSTRef
  | GhcMVar
  | GhcConcSync
  | GhcMaybe
  | GhcReal
  | DataEither

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
    go :: SPkg pkg -> KnownModule pkg -> FlatClosure -> Maybe (String, [Box])
    go knownPkg knownModl ConstrClosure{pkg, modl, name, ptrArgs} = do
        guard (namePkg knownPkg `isPrefixOf` pkg) -- ignore the version number
        guard (modl == nameModl knownPkg knownModl)
        return (name, ptrArgs)
    go _ _ _otherClosure = Nothing

    namePkg :: SPkg pkg -> String
    namePkg SGhcPrim             = "ghc-prim"
    namePkg SBase                = "base"
    namePkg SByteString          = "bytestring"
    namePkg SText                = "text"
    namePkg SIntegerWiredIn      = "integer-wired-in"
    namePkg SGhcBignum           = "ghc-bignum"
    namePkg SContainers          = "containers"
    namePkg SAeson               = "aeson"
    namePkg SUnorderedContainers = "unordered-containers"
    namePkg SVector              = "vector"

    nameModl :: SPkg pkg -> KnownModule pkg -> String
    nameModl SGhcPrim             GhcTypes                    = "GHC.Types"
    nameModl SGhcPrim             GhcTuple                    = "GHC.Tuple"
    nameModl SBase                GhcInt                      = "GHC.Int"
    nameModl SBase                GhcWord                     = "GHC.Word"
    nameModl SBase                GhcSTRef                    = "GHC.STRef"
    nameModl SBase                GhcMVar                     = "GHC.MVar"
    nameModl SBase                GhcConcSync                 = "GHC.Conc.Sync"
    nameModl SBase                GhcMaybe                    = "GHC.Maybe"
    nameModl SBase                GhcReal                     = "GHC.Real"
    nameModl SBase                DataEither                  = "Data.Either"
    nameModl SByteString          DataByteStringInternal      = "Data.ByteString.Internal"
    nameModl SByteString          DataByteStringLazyInternal  = "Data.ByteString.Lazy.Internal"
    nameModl SByteString          DataByteStringShortInternal = "Data.ByteString.Short.Internal"
    nameModl SText                DataTextInternal            = "Data.Text.Internal"
    nameModl SText                DataTextInternalLazy        = "Data.Text.Internal.Lazy"
    nameModl SIntegerWiredIn      GhcIntegerType              = "GHC.Integer.Type"
    nameModl SGhcBignum           GhcNumInteger               = "GHC.Num.Integer"
    nameModl SContainers          DataSetInternal             = "Data.Set.Internal"
    nameModl SContainers          DataMapInternal             = "Data.Map.Internal"
    nameModl SContainers          DataIntSetInternal          = "Data.IntSet.Internal"
    nameModl SContainers          DataIntMapInternal          = "Data.IntMap.Internal"
    nameModl SContainers          DataSequenceInternal        = "Data.Sequence.Internal"
    nameModl SContainers          DataTree                    = "Data.Tree"
    nameModl SAeson               DataAesonTypesInternal      = "Data.Aeson.Types.Internal"
    nameModl SUnorderedContainers DataHashMapInternal         = "Data.HashMap.Internal"
    nameModl SUnorderedContainers DataHashMapInternalArray    = "Data.HashMap.Internal.Array"
    nameModl SVector              DataVector                  = "Data.Vector"
