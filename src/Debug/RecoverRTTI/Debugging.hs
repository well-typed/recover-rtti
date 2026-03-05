-- | Debugging support
module Debug.RecoverRTTI.Debugging (
    -- * Tracing
    traceAnything
  , traceAnythingId
    -- * Deriving-via support
  , AnythingToString(..)
  ) where

import Debug.Trace

import Debug.RecoverRTTI.Classify

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Like 'traceShow', but using 'anythingToString'
traceAnything :: a -> b -> b
traceAnything a = trace (anythingToString a)

-- | Like 'traceShowId', but using 'anythingToString'
traceAnythingId :: a -> a
traceAnythingId a = trace (anythingToString a) a

{-------------------------------------------------------------------------------
  Deriving-via support
-------------------------------------------------------------------------------}

-- | Deriving-via support for 'anythingToString'
--
-- If for debugging purposes you want to temporarily add a 'Show' instance to
-- an arbitrary datatype in terms of 'anythingToString', you can do so using
--
-- > data T = MkT ...
-- >   deriving Show via AnythingToString T
--
-- This is equivalent to saying
--
-- > instance Show T where
-- >   show = anythingToString
newtype AnythingToString a = AnythingToString a

instance Show (AnythingToString a) where
  show (AnythingToString x) = anythingToString x

