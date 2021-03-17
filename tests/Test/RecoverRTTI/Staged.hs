{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

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
  , classifyThenReclassify
  ) where

import Control.Monad.Except
import Data.Bifunctor
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Kind
import Data.Map (Map)
import Data.Set (Set)
import Data.SOP hiding (NS(..))
import Data.Typeable
import Data.Void
import GHC.Exts (Any)
import GHC.Real
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.HashMap.Internal.Array as HashMap (Array)
import qualified Data.HashMap.Internal.Array as HashMap.Array
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set

import Debug.RecoverRTTI
import Debug.RecoverRTTI.TypeLevel

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.UserDefined

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

-- | Classify, then reclassify
classifyThenReclassify :: a -> Except String (Reclassified a)
classifyThenReclassify x =
    case classified x of
      Left closure ->
        throwError $ "Failed to classify closure " ++ show closure
      Right classifier ->
        reclassify classifier

-- | Reclassify values
--
-- See detailed description in 'Reclassified'.
reclassify :: Classified a -> Except String (Reclassified a)
reclassify = go
  where
    go :: Classified a -> Except String (Reclassified a)
    go (Classified c x) = case c of
      -- Primitive types

      C_Bool     -> return $ Reclassified CC_Bool     id
      C_Char     -> return $ Reclassified CC_Char     id
      C_Double   -> return $ Reclassified CC_Double   id
      C_Float    -> return $ Reclassified CC_Float    id
      C_Int      -> return $ Reclassified CC_Int      id
      C_Int8     -> return $ Reclassified CC_Int8     id
      C_Int16    -> return $ Reclassified CC_Int16    id
      C_Int32    -> return $ Reclassified CC_Int32    id
      C_Int64    -> return $ Reclassified CC_Int64    id
      C_Integer  -> return $ Reclassified CC_Integer  id
      C_Ordering -> return $ Reclassified CC_Ordering id
      C_Unit     -> return $ Reclassified CC_Unit     id
      C_Word     -> return $ Reclassified CC_Word     id
      C_Word8    -> return $ Reclassified CC_Word8    id
      C_Word16   -> return $ Reclassified CC_Word16   id
      C_Word32   -> return $ Reclassified CC_Word32   id
      C_Word64   -> return $ Reclassified CC_Word64   id

      -- String types

      C_String      -> return $ Reclassified CC_String      id
      C_BS_Strict   -> return $ Reclassified CC_BS_Strict   id
      C_BS_Lazy     -> return $ Reclassified CC_BS_Lazy     id
      C_BS_Short    -> return $ Reclassified CC_BS_Short    id
      C_Text_Strict -> return $ Reclassified CC_Text_Strict id
      C_Text_Lazy   -> return $ Reclassified CC_Text_Lazy   id

      -- Aeson

      C_Value -> return $ Reclassified CC_Value id

      -- Compound

      C_Maybe        c' -> goMaybeF     fmap          CC_Maybe        c'
      C_Either       c' -> goEitherF    bimap         CC_Either       c'
      C_List         c' -> goMaybeF     fmap          CC_List         c'
      C_Ratio        c' -> goF          coerceRatio   CC_Ratio        c'
      C_Set          c' -> goMaybeF     coerceSet     CC_Set          c'
      C_Map          c' -> goMaybePairF coerceMap     CC_Map          c'
      C_IntSet          -> return $ Reclassified CC_IntSet id
      C_IntMap       c' -> goMaybeF     fmap          CC_IntMap       c'
      C_Sequence     c' -> goMaybeF     fmap          CC_Sequence     c'
      C_Tree         c' -> goF          fmap          CC_Tree         c'
      C_HashSet      c' -> goF          coerceHashSet CC_HashSet      c'
      C_HashMap      c' -> goMaybePairF coerceHashMap CC_HashMap      c'
      C_HM_Array     c' -> goMaybeF     coerceHMArray CC_HM_Array     c'
      C_Vector_Boxed c' -> goMaybeF     fmap          CC_Vector_Boxed c'

      C_Tuple (Classifiers cs) ->
        reclassifyTuple <$> (hsequence' (hmap (Comp . reclassify) cs))

      -- Reference cells

      C_STRef -> return $ Reclassified CC_STRef id
      C_TVar  -> return $ Reclassified CC_TVar  id
      C_MVar  -> return $ Reclassified CC_MVar  id

      -- Functions

      C_Fun -> return $ Reclassified CC_Fun id

      -- User-defined

      C_Custom s ->
        firstMatch ("Unknown constructor: " ++ prettyKnownConstr s) [
            reclassifyF CC_User_NonRec   s x
          , reclassifyF CC_User_Rec      s x
          , reclassifyF CC_User_Unlifted s x
          ]

    goMaybeF :: forall f a.
         (forall x x'. (x -> x') -> f x -> f x')
      -> (forall x. MaybeF ConcreteClassifier x -> ConcreteClassifier (f x))
      -> MaybeF Classified a
      -> Except String (Reclassified (f a))
    goMaybeF _ cc FNothing =
        return $ Reclassified (cc FNothing) id
    goMaybeF coerce cc (FJust x') =
        aux <$> reclassify x'
      where
        aux :: Reclassified x -> Reclassified (f x)
        aux (Reclassified c_x f_x) =
            Reclassified (cc (FJust c_x)) (coerce f_x)

    goEitherF :: forall f a b.
         (forall x x' y y'. (x -> x') -> (y -> y') -> f x y -> f x' y')
      -> (forall x y. EitherF ConcreteClassifier x y -> ConcreteClassifier (f x y))
      -> EitherF Classified a b
      -> Except String (Reclassified (f a b))
    goEitherF coerce cc (FLeft x') =
        aux <$> reclassify x'
      where
        aux :: Reclassified x -> Reclassified (f x Void)
        aux (Reclassified c_x f_x) =
            Reclassified (cc (FLeft c_x)) (coerce f_x id)
    goEitherF coerce cc (FRight y') =
        aux <$> reclassify y'
      where
        aux :: Reclassified y -> Reclassified (f Void y)
        aux (Reclassified c_y f_y) =
            Reclassified (cc (FRight c_y)) (coerce id f_y)

    goMaybePairF :: forall f a b.
         (forall x x' y y'. (x -> x') -> (y -> y') -> f x y -> f x' y')
      -> (forall x y. MaybePairF ConcreteClassifier x y -> ConcreteClassifier (f x y))
      -> MaybePairF Classified a b
      -> Except String (Reclassified (f a b))
    goMaybePairF _ cc FNothingPair =
        return $ Reclassified (cc FNothingPair) id
    goMaybePairF coerce cc (FJustPair x' y') =
        aux <$> reclassify x' <*> reclassify y'
      where
        aux :: Reclassified x -> Reclassified y -> Reclassified (f x y)
        aux (Reclassified c_x f_x) (Reclassified c_y f_y) =
            Reclassified (cc (FJustPair c_x c_y)) (coerce f_x f_y)

    goF :: forall f a.
         (forall x x'. (x -> x') -> f x -> f x')
      -> (forall x. ConcreteClassifier x -> ConcreteClassifier (f x))
      -> Classified a
      -> Except String (Reclassified (f a))
    goF coerce cc x' =
        aux <$> reclassify x'
      where
        aux :: Reclassified x -> Reclassified (f x)
        aux (Reclassified c_x f_x) =
            Reclassified (cc c_x) (coerce f_x)

reclassifyTuple ::
     (SListI xs, IsValidSize (Length xs))
  => NP Reclassified xs -> Reclassified (WrappedTuple xs)
reclassifyTuple = \cs ->
    go cs $ \cs' f ->
      Reclassified (CC_Tuple (ConcreteClassifiers cs')) f
  where
    go :: forall xs r.
         (SListI xs, IsValidSize (Length xs))
      => NP Reclassified xs
      -> (forall ys.
               (SListI ys, Length ys ~ Length xs)
            => NP ConcreteClassifier ys
            -> (WrappedTuple xs -> WrappedTuple ys)
            -> r
         )
      -> r
    go Nil       k = k Nil id
    go (x :* xs) k = smallerIsValid (Proxy @(Length xs)) $
                       go xs $ \np f_np ->
                         case x of
                           Reclassified y f_y ->
                             k (y :* np) (bimapTuple f_y f_np)

{-------------------------------------------------------------------------------
  Lift coercions to non-functor types
-------------------------------------------------------------------------------}

coerceRatio :: (x -> x') -> Ratio x -> Ratio x'
coerceRatio f (x :% y) = f x :% f y

coerceSet :: (x -> x') -> Set x -> Set x'
coerceSet f = Set.fromDistinctAscList . map f . Set.toAscList

coerceMap :: (x -> x') -> (y -> y') -> Map x y -> Map x' y'
coerceMap f g = Map.fromDistinctAscList . map (bimap f g) . Map.toAscList

coerceHMArray :: (x -> x') -> HashMap.Array x -> HashMap.Array x'
coerceHMArray f arr =
    let xs = HashMap.Array.toList arr
    in HashMap.Array.fromList (length xs) (map f xs)

-- Unfortunately, coercion on HashSet/HashMap is not expressible using its API
coerceHashSet :: (x -> x') -> HashSet x -> HashSet x'
coerceHashSet _ = unsafeCoerce

coerceHashMap :: (x -> x') -> (y -> y') -> HashMap x y -> HashMap x' y'
coerceHashMap _ _ = unsafeCoerce

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
       , Typeable f
       )
  => (forall a. MaybeF ConcreteClassifier a -> ConcreteClassifier (f a))
  -> Sing (c :: Constr Symbol)
  -> UserDefined c
  -> Except String (Maybe (Reclassified (UserDefined c)))
reclassifyF cc = \c x ->
    case checkConstrOfF @f c of
      Nothing ->
        return Nothing
      Just constrOfF ->
        case checkEmptyTraversable (unsafeCoerceF constrOfF x) of
          Right _ ->
            return . Just $ Reclassified (cc FNothing) (unsafeCoerceF constrOfF)
          Left x' -> do
            Just . aux constrOfF <$> classifyThenReclassify x'
  where
    aux :: Poly (ConstrOfF f c)
        -> Reclassified a               -- Classification of the elements
        -> Reclassified (UserDefined c) -- Classification of the container
    aux constrOfF (Reclassified c f) =
        Reclassified (cc (FJust c)) (fmap f . unsafeCoerceF constrOfF)

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

instance Phantom (ConstrOfF ContainsUnlifted c) where
  phantom (ConstrOfF IsConstrOf) = ConstrOfF IsConstrOf

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

firstMatch :: forall e a. e -> [Except e (Maybe a)] -> Except e a
firstMatch err = go
  where
    go :: [Except e (Maybe a)] -> Except e a
    go []     = throwError err
    go (x:xs) = x >>= maybe (go xs) return

bimapTuple ::
      ( SListI xs
      , SListI ys
      , IsValidSize (Length (x ': xs))
      , Length xs ~ Length ys
      )
   => (x -> y)
   -> (WrappedTuple xs -> WrappedTuple ys)
   -> WrappedTuple (x ': xs) -> WrappedTuple (y ': ys)
bimapTuple f g (TCons x xs) = TCons (f x) (g xs)

-- | Check if a traversable data structure is empty
--
-- Returns evidence: an element of the data-structure if it's non-empty,
-- or evidence that it is empty otherwise.
checkEmptyTraversable :: Traversable t => t a -> Either a (t Void)
checkEmptyTraversable = traverse Left
