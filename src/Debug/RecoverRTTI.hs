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
    -- ** Mutable arrays
  , SomePrimMutableArray(..)
    -- * User-defined types
  , UserDefined -- opaque
    -- ** Classify constructor arguments
  , fromUserDefined
  , Some(..)
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
    -- * Type-level naturals
  , Nat(..)
  , SNat(..)
  , KnownNat(..)
  , Length
  ) where

import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Classify
import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Wrappers
