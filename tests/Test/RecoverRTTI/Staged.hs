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
    UserClassifier
  , classifyConcrete
  , reclassify
  ) where

import Control.Monad.Except
import Data.SOP hiding (NS(..))

import Debug.RecoverRTTI

import Test.RecoverRTTI.UserDefined

{-------------------------------------------------------------------------------
  Reclassified values
-------------------------------------------------------------------------------}

type UserClassifier = Classifier_ ClassifyUser

-- | Classify, then reclassify
classifyConcrete :: a -> Except String (Reclassified UserClassifier a)
classifyConcrete x =
    case classify x of
      Left closure ->
        throwError $ "Failed to classify closure " ++ show closure
      Right classifier ->
        reclassify classifier

-- | Reclassify values
--
-- See detailed description in 'Reclassified'.
reclassify :: Classifier a -> Except String (Reclassified UserClassifier a)
reclassify = fmap distribReclassified . reclassify_ go
  where
    go :: IsUserDefined a -> Except String (Reclassified ClassifyUser a)
    go (IsUserDefined x) =
        firstMatch ("Unknown constructor: " ++ constr) [
            goSimple      C_Simple    constr
          , goTraversable C_NonRec    constr
          , goTraversable C_Rec       constr
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
         forall f. ConstrsOf f
      => ClassifyUser (f Deferred)
      -> String
      -> Except String (Maybe (Reclassified ClassifyUser UserDefined))
    goTraversable c constr =
         if constr `notElem` constrsOf (Proxy @f)
           then return Nothing
           else return . Just $ Reclassified c FromUsr

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

firstMatch :: forall e a. e -> [Except e (Maybe a)] -> Except e a
firstMatch err = go
  where
    go :: [Except e (Maybe a)] -> Except e a
    go []     = throwError err
    go (x:xs) = x >>= maybe (go xs) return
