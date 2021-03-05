{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Staged inference
--
-- Suppose we have a user-defined type such as
--
-- > data T a = MkT a
--
-- When we classify a value of type @T a@, 'classify' will give us "one level"
-- type inference only: it will classify this value as
--
-- > UserDefined (Constr "pkg" "modl" "MkT")
--
-- It will not attempt to classify the /arguments/ to the constructor. If we
-- /know/ which user-defined types we're interested in, however, we can do
-- full classification by doing "staged inference", repeatedly calling
-- 'classify' at every level.
--
-- In this module we do staged inference for the user-defined types used in the
-- test suite. The primary purpose of this is to provide evidence that
-- 'classify' gives us enough information to do so.
module Test.RecoverRTTI.Staged (
    Reclassified(..)
  , reclassify
  ) where

import Data.Foldable (asum)
import Data.Kind
import GHC.Exts (Any)
import GHC.TypeLits

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Util.TypeLevel

import Test.RecoverRTTI.Arbitrary

{-------------------------------------------------------------------------------
  Reclassified values
-------------------------------------------------------------------------------}

-- | Reclassified values
--
-- We cannot go directly from a @Classifier a@ to a @ConcreteClassifier a@:
-- in the case of a user-defined type, @a@ will be of the form
--
-- > UserDefined c
--
-- for some @c@, but we want to return classifier for a specific type, maybe
--
-- > NonRecursive Char
--
-- Therefore instead we return a classifier for some other type @b@, but along
-- with a proof that we can /coerce/ from @a@ to @b@.
data Reclassified a where
    Reclassified :: ConcreteClassifier b -> (a -> b) -> Reclassified a

-- | Reclassify values
--
-- See detailed description in 'Reclassified'.
reclassify :: Classified a -> Maybe (Reclassified a)
reclassify = \(Classified c x) ->
    case c of
      -- Primitive types

      C_Bool     -> Just $ Reclassified CC_Bool     id
      C_Char     -> Just $ Reclassified CC_Char     id
      C_Double   -> Just $ Reclassified CC_Double   id
      C_Float    -> Just $ Reclassified CC_Float    id
      C_Int      -> Just $ Reclassified CC_Int      id
      C_Int8     -> Just $ Reclassified CC_Int8     id
      C_Int16    -> Just $ Reclassified CC_Int16    id
      C_Int32    -> Just $ Reclassified CC_Int32    id
      C_Int64    -> Just $ Reclassified CC_Int64    id
      C_Ordering -> Just $ Reclassified CC_Ordering id
      C_Unit     -> Just $ Reclassified CC_Unit     id
      C_Word     -> Just $ Reclassified CC_Word     id
      C_Word8    -> Just $ Reclassified CC_Word8    id
      C_Word16   -> Just $ Reclassified CC_Word16   id
      C_Word32   -> Just $ Reclassified CC_Word32   id
      C_Word64   -> Just $ Reclassified CC_Word64   id

      -- Compound

      C_List Empty -> Just $ Reclassified (CC_List Empty) id
      C_List (NonEmpty x') -> cc_list <$> reclassify x'

      -- User-defined

      C_Custom (s :: Sing c) -> asum [
          reclassifyF CC_User_NonRec s x
        , reclassifyF CC_User_Rec    s x
        ]


      -- Classification failed

      C_Unknown -> Nothing
  where
    cc_list :: Reclassified a -> Reclassified [a]
    cc_list (Reclassified c f) = Reclassified (CC_List (NonEmpty c)) (map f)

{-------------------------------------------------------------------------------
  When we reclassify values of user-defined types with type arguments, we need
  to know that if @c@ is a value of, say, @T a@, it is also a value of @T b@,
  for all @b@. This is what enables staged inference: we know it's a constructor
  of @T x@ for /some/ @x@, and then as a second step figure out what @x@ is.
-------------------------------------------------------------------------------}

data ConstrOfF f c a = ConstrOfF (IsConstrOf (f a) c)

unsafeCoerceF :: Poly (ConstrOfF f c) -> UserDefined c -> f a
unsafeCoerceF (Poly (ConstrOfF isConstrOf)) = aux isConstrOf
  where
    aux :: forall a c. IsConstrOf a c -> UserDefined c -> a
    aux IsConstrOf = unsafeCoerceUserDefined

checkConstrOfF ::
     forall f c. (
         Phantom (ConstrOfF f c)
       , SingI (Constrs (f Any))
       )
  => Sing c -> Maybe (Poly (ConstrOfF f c))
checkConstrOfF c = maybePoly (ConstrOfF <$> checkIsConstrOf @(f Any) c)

reclassifyF ::
     forall (f :: Type -> Type) (c :: Constr Symbol). (
         Phantom (ConstrOfF f c)
       , SingI (Constrs (f Any))
       , Traversable f
       )
  => (forall a. MaybeEmpty ConcreteClassifier a -> ConcreteClassifier (f a))
  -> Sing (c :: Constr Symbol)
  -> UserDefined c
  -> Maybe (Reclassified (UserDefined c))
reclassifyF cc = \c x -> do
    constrOfF <- checkConstrOfF @f c
    case checkEmptyTraversable (unsafeCoerceF constrOfF x) of
      Right _ -> return $ Reclassified (cc Empty) (unsafeCoerceF constrOfF)
      Left x' -> aux constrOfF <$> reclassify (classified x')
  where
    aux :: Poly (ConstrOfF f c)
        -> Reclassified a               -- Classification of the elements
        -> Reclassified (UserDefined c) -- Classification of the container
    aux constrOfF (Reclassified c f) =
        Reclassified (cc (NonEmpty c)) (fmap f . unsafeCoerceF constrOfF)

{-------------------------------------------------------------------------------
  Prove that the functors of our user-defined types are indeed parametric

  NOTE: It's kinda frustrating that we have to repeat this for every type.
  That's non-trivial to fix though; a polymorphic function would need as
  quantified constraint that

  > forall a b. Constrs (f a) ~ Constrs (f b)

  but that is not legal Haskell: we cannot use type synonyms in quantified
  constraints (frustratingly and unnecessarily).
-------------------------------------------------------------------------------}

instance Phantom (ConstrOfF NonRecursive c) where
  phantom (ConstrOfF IsConstrOf) = ConstrOfF IsConstrOf

instance Phantom (ConstrOfF Recursive c) where
  phantom (ConstrOfF IsConstrOf) = ConstrOfF IsConstrOf
