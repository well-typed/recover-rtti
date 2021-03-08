{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | User-defined types
--
-- For user-defined types, we have no way of actually inferring the /type/ at
-- runtime. Instead, we merely reflect (at the type level) which /constructor/
-- was used to construct the value. This gives the library sufficient context
-- to /show/ values, and it gives client code that /is/ aware of these custom
-- types sufficient context to recover the full type information, if desired.
-- See "Test.RecoverRTTI.Staged" for an example of the latter.
module Debug.RecoverRTTI.UserDefined (
    -- | Type inferred for user-defined types
    UserDefined(..)
  , unsafeCoerceUserDefined
  ) where

import Data.Proxy
import GHC.TypeLits
import GHC.Exts
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.Util

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

-- | Safer wrapper around 'unsafeCoerce'
--
-- This is safer than 'unsafeCoerce', because we require (at the type level)
-- that the value was constructed with a constructor of the target type. This
-- means that 'unsafeCoerceUserDefined' is in fact /safe/ for types without
-- type parameters; however, for a type such as
--
-- > data MyType a = MkMyType a
--
-- 'unsafeCoerceUserDefined' can still be used to cast, say, @MyType Int@ to
-- @MyType Bool@, and so this is still unsafe.
unsafeCoerceUserDefined :: forall a c. ConstrOf a c => UserDefined c -> a
unsafeCoerceUserDefined = unsafeCoerce
  where
    _ = keepRedundantConstraint (Proxy @(ConstrOf a c))
