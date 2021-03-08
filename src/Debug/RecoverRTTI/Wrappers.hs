-- | Newtype wrappers
--
-- For some types, we infer a "wrapped type" instead. It is important that
-- these wrappers are all /newtypes/ and not /datatypes, because when we have
-- a value @x :: a@ and a corresponding classifier @Classifier a@, we should be
-- able to @unsafeCoerce x@ to whatever type the classifier tells us. If we
-- don't use newtypes but datatypes here, then that would not be possible.
--
-- We use these newtypes primarily so that we can give some custom type class
-- instances. In particular, this means that for example our inferred functions
-- have a show instance, even if they are simply shown as @<Fun>@; this is
-- nonetheless stil useful, as it means that we can show /everything/, which is
-- kind of the point.
module Debug.RecoverRTTI.Wrappers (
    -- * Functions
    SomeFun(..)
    -- * Reference cells
  , SomeSTRef(..)
  , SomeMVar(..)
  , SomeTVar(..)
    -- * Values of an unknown type (failed classification)
  , Unknown(..)
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Data.STRef (STRef)
import GHC.Exts
import GHC.Exts.Heap
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

-- | Functions
--
-- We do not try to infer the domain or codomain of the function.
newtype SomeFun = SomeFun (Any -> Any)

{-------------------------------------------------------------------------------
  Reference cells

  We do not try to look inside these variables to figure out the type of their
  elements; the show instance merely shows an address.
-------------------------------------------------------------------------------}

newtype SomeSTRef = SomeSTRef (STRef Any Any)
  deriving (Eq)

newtype SomeMVar = SomeMVar (MVar Any)
  deriving (Eq)

newtype SomeTVar = SomeTVar (TVar Any)
  deriving (Eq)

{-------------------------------------------------------------------------------
  Unknown values
-------------------------------------------------------------------------------}

-- | Value for which type inference failed
newtype Unknown = Unknown Any

{-------------------------------------------------------------------------------
  Show instances

  Unfortunately reference cells are moved by GC, so we can't do much here;
  showing the address of the variable isn't particularly helpful.
-------------------------------------------------------------------------------}

instance Show SomeSTRef where
  show _ = "<STRef/IORef>" -- they look the same on the heap

instance Show SomeMVar where
  show _ = "<MVar>"

instance Show SomeTVar where
  show _ = "<TVar>"

instance Show SomeFun where
  show _ = "<Fun>"

-- | If classification failed, we show the closure itself
instance Show Unknown where
  showsPrec p (Unknown x) = unsafePerformIO $ do
      closure <- getBoxedClosureData (asBox x)
      return $ showsPrec p closure
