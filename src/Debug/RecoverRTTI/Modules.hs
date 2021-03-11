{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

-- | Modules we recognize types from
module Debug.RecoverRTTI.Modules (
    KnownPkg(..)
  , KnownModule(..)
  , Sing(..)
    -- * Matching
  , inKnownModule
  , inKnownModuleNested
  ) where

import Control.Monad
import Data.List (isPrefixOf)

import Debug.RecoverRTTI.FlatClosure
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Packages
-------------------------------------------------------------------------------}

data KnownPkg =
    PkgGhcPrim
  | PkgBase
  | PkgByteString
  | PkgText
  | PkgIntegerWiredIn

data family KnownModule (pkg :: KnownPkg)

{-------------------------------------------------------------------------------
  Singleton instance for KnownPkg
-------------------------------------------------------------------------------}

data instance Sing (pkg :: KnownPkg) where
  SGhcPrim        :: Sing 'PkgGhcPrim
  SBase           :: Sing 'PkgBase
  SByteString     :: Sing 'PkgByteString
  SText           :: Sing 'PkgText
  SIntegerWiredIn :: Sing 'PkgIntegerWiredIn

instance SingI 'PkgGhcPrim        where sing = SGhcPrim
instance SingI 'PkgBase           where sing = SBase
instance SingI 'PkgByteString     where sing = SByteString
instance SingI 'PkgText           where sing = SText
instance SingI 'PkgIntegerWiredIn where sing = SIntegerWiredIn

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
  Matching
-------------------------------------------------------------------------------}

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
    namePkg SGhcPrim        = "ghc-prim"
    namePkg SBase           = "base"
    namePkg SByteString     = "bytestring"
    namePkg SText           = "text"
    namePkg SIntegerWiredIn = "integer-wired-in"

    nameModl :: Sing (pkg :: KnownPkg) -> KnownModule pkg -> String
    nameModl SGhcPrim        GhcTypes                    = "GHC.Types"
    nameModl SGhcPrim        GhcTuple                    = "GHC.Tuple"
    nameModl SBase           GhcInt                      = "GHC.Int"
    nameModl SBase           GhcWord                     = "GHC.Word"
    nameModl SBase           GhcSTRef                    = "GHC.STRef"
    nameModl SBase           GhcMVar                     = "GHC.MVar"
    nameModl SBase           GhcConcSync                 = "GHC.Conc.Sync"
    nameModl SBase           GhcMaybe                    = "GHC.Maybe"
    nameModl SBase           GhcReal                     = "GHC.Real"
    nameModl SBase           DataEither                  = "Data.Either"
    nameModl SByteString     DataByteStringInternal      = "Data.ByteString.Internal"
    nameModl SByteString     DataByteStringLazyInternal  = "Data.ByteString.Lazy.Internal"
    nameModl SByteString     DataByteStringShortInternal = "Data.ByteString.Short.Internal"
    nameModl SText           DataTextInternal            = "Data.Text.Internal"
    nameModl SText           DataTextInternalLazy        = "Data.Text.Internal.Lazy"
    nameModl SIntegerWiredIn GhcIntegerType              = "GHC.Integer.Type"
