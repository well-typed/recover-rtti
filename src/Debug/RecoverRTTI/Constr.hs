{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Type-level metadata
module Debug.RecoverRTTI.Constr (
    Constr(..)
  , KnownConstr
  , knownConstr
  , prettyKnownConstr
  , elimKnownConstr
    -- | Type-level extracts
  , ConstrPkg
  , ConstrModl
  , ConstrName
    -- | Compute all known constructors
  , Constrs
  , GConstrs
  , GConstrsOfType
  , ConstrOf
  , IsConstrOf(..)
  , checkIsConstrOf
  , Sing(..)
  ) where

import Data.Kind
import Data.List (intercalate)
import Data.SOP
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  Type-level metadata
-------------------------------------------------------------------------------}

-- | Information about a constructor
data Constr a = Constr {
      constrPkg  :: a  -- ^ Package
    , constrModl :: a  -- ^ Module
    , constrName :: a  -- ^ Constructor name
    }
  deriving (Show, Eq)

type family ConstrPkg  (c :: Constr a) :: a where ConstrPkg  ('Constr p _ _) = p
type family ConstrModl (c :: Constr a) :: a where ConstrModl ('Constr _ m _) = m
type family ConstrName (c :: Constr a) :: a where ConstrName ('Constr _ _ c) = c

class (
      KnownSymbol (ConstrPkg  c)
    , KnownSymbol (ConstrModl c)
    , KnownSymbol (ConstrName c)
    , c ~ 'Constr (ConstrPkg c) (ConstrModl c) (ConstrName c)
    ) => KnownConstr c
instance (
      KnownSymbol (ConstrPkg  c)
    , KnownSymbol (ConstrModl c)
    , KnownSymbol (ConstrName c)
    , c ~ 'Constr (ConstrPkg c) (ConstrModl c) (ConstrName c)
    ) => KnownConstr c

knownConstr ::
     forall c. Sing (c :: Constr Symbol) -> Constr String
knownConstr SConstr = Constr {
      constrPkg  = symbolVal (Proxy @(ConstrPkg  c))
    , constrModl = symbolVal (Proxy @(ConstrModl c))
    , constrName = symbolVal (Proxy @(ConstrName c))
    }

prettyKnownConstr :: Sing (c :: Constr Symbol) -> String
prettyKnownConstr s = intercalate "." [constrPkg, constrModl, constrName]
  where
    Constr{constrPkg, constrModl, constrName} = knownConstr s

elimKnownConstr :: forall r.
     Constr String
  -> (forall c. Sing (c :: Constr Symbol) -> r)
  -> r
elimKnownConstr Constr{constrPkg, constrModl, constrName} k =
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
       => Proxy pkg -> Proxy modl -> Proxy constr -> r
    go _ _ _ = k (sing :: Sing ('Constr pkg modl constr))

{-------------------------------------------------------------------------------
  Singleton
-------------------------------------------------------------------------------}

data instance Sing (c :: Constr Symbol) where
  SConstr :: KnownConstr c => Sing c

instance KnownConstr c => SingI (c :: Constr Symbol) where
  sing = SConstr

instance Show (Sing (c :: Constr Symbol)) where
  showsPrec p proxy = showParen (p >= 11) $
        showString "SConstr "
      . showsPrec 11 (knownConstr proxy)

instance DecidableEquality (Constr Symbol) where
  decideEquality :: forall c c'.
       Sing (c  :: Constr Symbol)
    -> Sing (c' :: Constr Symbol)
    -> Maybe (c :~: c')
  decideEquality SConstr SConstr =
    case ( decideEquality (sing :: Sing (ConstrPkg  c)) (sing :: Sing (ConstrPkg  c'))
         , decideEquality (sing :: Sing (ConstrModl c)) (sing :: Sing (ConstrModl c'))
         , decideEquality (sing :: Sing (ConstrName c)) (sing :: Sing (ConstrName c'))
         ) of
      (Just Refl, Just Refl, Just Refl) ->
        Just Refl
      _otherwise ->
        Nothing

{-------------------------------------------------------------------------------
  Reverse direction: from type to the known constructors
-------------------------------------------------------------------------------}

-- | Compute all constructors of the given type
type family Constrs (a :: Type) :: [Constr Symbol] where
  Constrs a = GConstrs (Rep a)

type family GConstrs f :: [Constr Symbol] where
  GConstrs (M1 D ('MetaData typ modl pkg isNewtype) f) =
    GConstrsOfType pkg modl f '[]

type family GConstrsOfType pkg modl f acc :: [Constr Symbol] where
  GConstrsOfType pkg modl (f :+: g) acc =
    GConstrsOfType pkg modl g (GConstrsOfType pkg modl f acc)
  GConstrsOfType pkg modl (M1 C ('MetaCons constr fixity isRecord) f) acc =
    'Constr pkg modl constr ': acc

-- | Require that specified type has the given constructor
--
-- Intended usage:
--
-- > castUserDefined :: forall c a. ConstrOf a c => UserDefined c -> a
-- > castUserDefined = unsafeCoerce
type family ConstrOf (a :: Type) (c :: Constr Symbol) :: Constraint where
  ConstrOf a c =
      Assert
        (Elem c (Constrs a))
        (TypeError (
                  (       'ShowType c
                    ':<>: 'Text " is not a valid constructor of "
                    ':<>: 'ShowType a
                  )
            ':$$: (       'Text "Valid constructors are: "
                    ':<>: 'ShowType (Constrs a)
                  )
          ))

-- | Evidence that @c@ is a constructor of @a@
data IsConstrOf (a :: Type) (c :: Constr Symbol) where
  IsConstrOf :: Elem c (Constrs a) ~ 'True => IsConstrOf a c

-- | Check if @c@ is a constructof of @a@
checkIsConstrOf :: forall (a :: Type) (c :: Constr Symbol).
      SingI (Constrs a)
   => Sing c -> Maybe (IsConstrOf a c)
checkIsConstrOf s =
    aux <$> checkIsElem s (sing @_ @(Constrs a))
  where
    aux :: IsElem c (Constrs a) -> IsConstrOf a c
    aux IsElem = IsConstrOf
