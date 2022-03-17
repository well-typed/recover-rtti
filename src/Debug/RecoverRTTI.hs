-- | Recover runtime type information
module Debug.RecoverRTTI (
    -- * Take advance of the recovered type information
    anythingToString
    -- * Debugging support
    -- ** Tracing
  , traceAnything
  , traceAnythingId
    -- ** Deriving-via
  , AnythingToString(..)
  , BoxAnything(..)
    -- * Recover type information
  , classify
  , Classifier
  , PrimClassifier(..)
  , IsUserDefined(..)
    -- ** Generalizations
  , Classifier_(..)
    -- ** Unknown or partially known type arguments
  , Elem(..)
  , Elems(..)
    -- ** Newtype wrappers for unshowable types
  , SomeSTRef(..)
  , SomeTVar(..)
  , SomeMVar(..)
  , SomeFun(..)
    -- ** Mutable arrays
  , SomePrimArrayM(..)
  , SomeStorableVector(..)
  , SomeStorableVectorM(..)
  , SomePrimitiveVector(..)
  , SomePrimitiveVectorM(..)
    -- * Working with classifiers
    -- ** Mapping
  , mapClassifier
    -- ** Equality
  , samePrim
  , sameClassifier_
  , sameElem
  , sameElems
    -- * User-defined types
  , UserDefined -- opaque
    -- ** Classify constructor arguments
  , Classified(..)
  , fromUserDefined
    -- * Recovering type class instances
    -- ** Show
  , canShowClassified
  , canShowPrim
  , canShowClassified_
    -- ** Generic
  , PrimSatisfies
  , primSatisfies
  , ClassifiedSatisfies
  , classifiedSatisfies
    -- * Reclassification
  , Reclassified(..)
  , reclassify_
  , distribReclassified
  , FromUsr(..)
  , coerceFromUsr
    -- * Inductive tuples
  , WrappedTuple(WrappedTuple, TNil, TCons)
  , unwrapTuple
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
    -- * Util
    -- ** Type-level naturals
  , Nat(..)
  , SNat(..)
  , KnownNat(..)
  , Length
    -- ** Existentials
  , Some(..)
  , mapSome
  ) where

import Debug.RecoverRTTI.CheckSame
import Debug.RecoverRTTI.Classifier
import Debug.RecoverRTTI.Classify
import Debug.RecoverRTTI.Constraint
import Debug.RecoverRTTI.Debugging
import Debug.RecoverRTTI.Nat
import Debug.RecoverRTTI.Reclassify
import Debug.RecoverRTTI.Tuple
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Wrappers
