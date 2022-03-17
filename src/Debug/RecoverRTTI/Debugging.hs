{-# LANGUAGE ExistentialQuantification #-}

-- | Debugging support
module Debug.RecoverRTTI.Debugging (
    -- * Tracing
    traceAnything
  , traceAnythingId
    -- * Deriving-via support
  , AnythingToString(..)
  , BoxAnything(..)
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

-- | Add level of indirection on the heap
--
-- (Advanced users only, for most use cases this should not be necessary.)
--
-- Type recovery in @recover-rtti@ (through 'classify') works by looking at the
-- values on the heap. For example, if we see a list, we then look at the first
-- element of that list (if any), and if that element happens to be an 'Int',
-- the inferred type is @[Int]@. When we show such a list ('anythingToString'),
-- every element of the list is interpreted as an 'Int', without doing further
-- type recovery.
--
-- This works for normal use cases, but fails in low-level code that uses 'Any'
-- to squeeze values of different types into a data structure not designed for
-- that purpose. For example, consider
--
-- > data T f = T [f Any]
--
-- If we call 'anythingToString' on a 'T' value with elements of different
-- types in the list, we get some unexpected results:
--
-- >    anythingToString (T [unsafeCoerce (1 :: Int), unsafeCoerce False])
-- > == "T [1,14355032]"
--
-- The reason is that the type of the list was inferred as @[Int]@, and hence
-- the 'Bool' was subsequently also interpreted as an 'Int'.
--
-- 'BoxAnything' helps to resolve the problem. There are ways in which it can
-- be used. First, we can derive the following entirely reasonable 'Show'
-- instance for 'T':
--
-- > deriving instance Show a => Show (T (K a))
--
-- We then get
--
-- >    show (T [K $ BoxAnything (1 :: Int), K $ BoxAnything False])
-- > == "T [K 1,K False]"
--
-- Alternatively, we can omit the 'Show' instance for 'T', to get
--
-- >    anythingToString (T [K $ BoxAnything (1 :: Int), K $ BoxAnything False])
-- > == "T [BoxAnything 1,BoxAnything False]"
--
-- For this second use case to work, it is critical that 'BoxAnything' is a
-- datatype, not a newtype, so that it actually appears on the heap.
data BoxAnything = forall a. BoxAnything a

instance Show BoxAnything where
  show (BoxAnything x) = anythingToString x
