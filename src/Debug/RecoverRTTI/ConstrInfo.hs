{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level metadata
module Debug.RecoverRTTI.ConstrInfo (
    ConstrInfo(..)
  , KnownConstr
  , knownConstr
  , someKnownConstr
  , elimKnownConstr
  , ProxyKnownConstr(..)
  ) where

import Data.SOP
import GHC.TypeLits

import Debug.RecoverRTTI.Util

{-------------------------------------------------------------------------------
  Type-level metadata
-------------------------------------------------------------------------------}

data ConstrInfo a = ConstrInfo {
      constrPkg  :: a
    , constrModl :: a
    , constrName :: a
    }
  deriving (Show, Eq)

type family ConstrPkg (c :: ConstrInfo a) :: a where
  ConstrPkg ('ConstrInfo p _ _) = p

type family ConstrModl (c :: ConstrInfo a) :: a where
  ConstrModl ('ConstrInfo _ m _) = m

type family ConstrName (c :: ConstrInfo a) :: a where
  ConstrName ('ConstrInfo _ _ c) = c

class (
      KnownSymbol (ConstrPkg  c)
    , KnownSymbol (ConstrModl c)
    , KnownSymbol (ConstrName c)
    ) => KnownConstr c
instance (
      KnownSymbol (ConstrPkg  c)
    , KnownSymbol (ConstrModl c)
    , KnownSymbol (ConstrName c)
    ) => KnownConstr c

knownConstr ::
     forall proxy c. KnownConstr c
  => proxy (c :: ConstrInfo Symbol) -> ConstrInfo String
knownConstr _ = ConstrInfo {
      constrPkg  = symbolVal (Proxy @(ConstrPkg  c))
    , constrModl = symbolVal (Proxy @(ConstrModl c))
    , constrName = symbolVal (Proxy @(ConstrName c))
    }

someKnownConstr :: ConstrInfo String -> Some ProxyKnownConstr
someKnownConstr ConstrInfo{constrPkg, constrModl, constrName} =
    elimKnownSymbol constrPkg  $ \pPkg  ->
    elimKnownSymbol constrModl $ \pModl ->
    elimKnownSymbol constrName $ \pName ->
      go pPkg pModl pName
  where
    go :: forall pkg modl constr. (
              KnownSymbol pkg
            , KnownSymbol modl
            , KnownSymbol constr
            )
       => Proxy pkg -> Proxy modl -> Proxy constr -> Some ProxyKnownConstr
    go _ _ _ = Exists (ProxyKnownConstr @('ConstrInfo pkg modl constr))

elimKnownConstr ::
     ConstrInfo String
  -> (forall c. ProxyKnownConstr c -> r)
  -> r
elimKnownConstr info k =
    case someKnownConstr info of
      Exists p -> k p

data ProxyKnownConstr (c :: ConstrInfo Symbol) where
    ProxyKnownConstr :: KnownConstr c => ProxyKnownConstr c

instance Show (ProxyKnownConstr c) where
  showsPrec p proxy@ProxyKnownConstr = showParen (p >= 11) $
        showString "ProxyKnownConstr "
      . showsPrec 11 (knownConstr proxy)
