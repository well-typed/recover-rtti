-- | Recover runtime type information
module Debug.RecoverRTTI (
    -- * Take advance of the recovered type information
    anythingToString
    -- * Recover type information
  , classify
  , Classifier(..)
  , Classifiers(..)
    -- ** Pair value with its classifier
  , Classified(..)
  , classified
    -- ** Unknown or partially known type arguments
  , MaybeF(..)
  , EitherF(..)
  , MaybePairF(..)
    -- ** Newtype wrappers for unshowable types
  , SomeSTRef(..)
  , SomeTVar(..)
  , SomeMVar(..)
  , SomeFun(..)
    -- * User-defined types
  , UserDefined -- opaque
    -- ** Classify constructor arguments
  , KnownConstr
  , fromUserDefined
  , Some(..)
    -- ** Constructor information
  , Constr(..)
    -- ** Type-level constructor information
  , ConstrPkg
  , ConstrModl
  , ConstrName
  , prettyKnownConstr
    -- ** Casting
  , unsafeCoerceUserDefined
  , ConstrOf
  , Constrs
  , GConstrs
  , GConstrsOfType
    -- ** Constructor check
  , checkIsConstrOf
  , IsConstrOf(..)
    -- * Inductive tuples
  , WrappedTuple(..)
  , Tuple
    -- ** Translation to/from NP
  , tupleFromNP
  , tupleToNP
    -- ** Valid tuple size
  , IsValidSize(..)
  , ValidSize(..)
  , TooBig(..)
  , smallerIsValid
  , toValidSize
  , liftValidSize
  ) where

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Classify
import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.UserDefined
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Wrappers
