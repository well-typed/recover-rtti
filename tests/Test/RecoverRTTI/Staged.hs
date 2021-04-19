{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
module Test.RecoverRTTI.Staged (classifyConcrete, reclassify) where

import Control.Monad.Except
import Data.SOP hiding (NS(..))
import Data.Void
import Unsafe.Coerce (unsafeCoerce)

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Classify

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.UserDefined

{-------------------------------------------------------------------------------
  Reclassified values
-------------------------------------------------------------------------------}

-- | Classify, then reclassify
classifyConcrete :: a -> Except String (Reclassified ConcreteClassifier a)
classifyConcrete x =
    case classify x of
      Left closure ->
        throwError $ "Failed to classify closure " ++ show closure
      Right classifier ->
        reclassify classifier

-- | Reclassify values
--
-- See detailed description in 'Reclassified'.
reclassify :: Classifier a -> Except String (Reclassified ConcreteClassifier a)
reclassify = fmap distribReclassified . reclassify_ go
  where
    go :: IsUserDefined a -> Except String (Reclassified ClassifyUser a)
    go (IsUserDefined x) =
        firstMatch ("Unknown constructor: " ++ constr) [
            goSimple      C_Simple    constr
          , goTraversable C_NonRec   (constr, x)
          , goTraversable C_Rec      (constr, x)
          , goSimple      C_Unlifted  constr
          ]
      where
        (constr, _args) = fromUserDefined x

    -- Reclassification of user-defined types with no arguments
    goSimple ::
         forall a. ConstrsOf a
      => ClassifyUser a
      -> String
      -> Except String (Maybe (Reclassified ClassifyUser UserDefined))
    goSimple c constr =
        if constr `notElem` constrsOf (Proxy @a)
          then return Nothing
          else return . Just $ Reclassified c FromUsr

    goTraversable ::
         forall f. (Traversable f, ConstrsOf f)
      => (forall a. Elems ClassifyUser '[a] -> ClassifyUser (f a))
      -> (String, UserDefined)
      -> Except String (Maybe (Reclassified ClassifyUser UserDefined))
    goTraversable cc = \(constr, x) ->
        if constr `notElem` constrsOf (Proxy @f) then
          return Nothing
        else
          case checkEmptyTraversable (coerceToF x) of
            Right _ -> return . Just $ Reclassified (cc ElemU) FromUsr
            Left x' -> Just . aux <$> classifyConcrete x'
      where
        coerceToF :: forall a. UserDefined -> f a
        coerceToF = unsafeCoerce

        aux ::
             Reclassified ConcreteClassifier a
          -> Reclassified ClassifyUser UserDefined
        aux (Reclassified c pf) =
            Reclassified (cc (ElemK c)) (F1 pf `Compose` FromUsr)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

firstMatch :: forall e a. e -> [Except e (Maybe a)] -> Except e a
firstMatch err = go
  where
    go :: [Except e (Maybe a)] -> Except e a
    go []     = throwError err
    go (x:xs) = x >>= maybe (go xs) return

-- | Check if a traversable data structure is empty
--
-- Returns evidence: an element of the data-structure if it's non-empty,
-- or evidence that it is empty otherwise.
checkEmptyTraversable :: Traversable t => t a -> Either a (t Void)
checkEmptyTraversable = traverse Left
