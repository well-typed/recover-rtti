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
    -- * User-defined types
    UserDefined(..)
    -- * Functions
  , SomeFun(..)
    -- * Reference cells
  , SomeSTRef(..)
  , SomeMVar(..)
  , SomeTVar(..)
    -- * Arrays
  , SomePrimArrayM(..)
  , SomeStorableVector(..)
  , SomeStorableVectorM(..)
  , SomePrimitiveVector(..)
  , SomePrimitiveVectorM(..)
  , SomeMutableByteArray(..)
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Data.STRef (STRef)
import GHC.Exts

import qualified Data.Primitive.Array     as Prim (MutableArray)
import qualified Data.Primitive.ByteArray as Prim (MutableByteArray)

{-------------------------------------------------------------------------------
  User-defined types
-------------------------------------------------------------------------------}

-- | User-defined type
--
-- We defer classification of the arguments to the constructor (the type might
-- be recursive, so if we tried to classify all arguments, we might end up
-- unrolling the recursion at the type level).
newtype UserDefined = UserDefined Any

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
  Arrays

  For mutable arrays, we don't currently peek inside, and so we don't infer
  the type of the elements. We /could/ (in principle this would be sound),
  and we may wish to change this at some point. For the purposes of /show/,
  however, having this type inferred it's particularly useful, unless we define
  a `peekAnythingToString :: a -> IO String` function or something like that
  which could look inside mutable structures (references, arrays, ..).
-------------------------------------------------------------------------------}

newtype SomePrimArrayM = SomePrimArrayM (Prim.MutableArray RealWorld Any)

-- | Storable vector ("Data.Vector.Storable")
--
-- For storable arrays we have no hope of inferring the type of the elements:
-- the elements are not stored as pointers, but rather as " serialized " data
-- through the 'Storable' type class. In order to get at any element, we'd need
-- to have the corresponding 'Storable' instance, but of course we don't have it
-- if we don't have the type.
newtype SomeStorableVector = SomeStorableVector Any

-- | Mutable storage vector ("Data.Vector.Storable")
--
-- See 'SomeStorableVector' for some details on why we don't infer anything here.
newtype SomeStorableVectorM = SomeStorableVectorM Any

-- | Primitive vector ("Data.Vector.Primitive")
--
-- See 'SomeStorableVector' for why we can't classify elements of these vectors.
newtype SomePrimitiveVector = SomePrimitiveVector Any

-- | Mutable primitive vector
newtype SomePrimitiveVectorM = SomePrimitiveVectorM Any

-- | Mutable byte array
newtype SomeMutableByteArray = SomeMutableByteArray (Prim.MutableByteArray RealWorld)

{-------------------------------------------------------------------------------
  Show instances

  Unfortunately reference cells are moved by GC, so we can't do much here;
  showing the address of the variable isn't particularly helpful.

  We /could/ use @unsafePerformIO@ here to display the /contents/ of these
  variables and mutable arrays, but that would not be referentially transparent;
  unlike 'classify', that would not be morally pure. In principle we could offer
  such a "display and peek inside mutable references" as a separate function.
-------------------------------------------------------------------------------}

instance Show SomeSTRef where
  show _ = "<STRef/IORef>" -- they look the same on the heap

instance Show SomeMVar where
  show _ = "<MVar>"

instance Show SomeTVar where
  show _ = "<TVar>"

instance Show SomeFun where
  show _ = "<Fun>"

instance Show SomePrimArrayM where
  show _ = "<Data.Primitive.Array.MutableArray>"

instance Show SomeStorableVector where
  show _ = "<Data.Vector.Storable.Vector>"

instance Show SomeStorableVectorM where
  show _ = "<Data.Vector.Storable.MVector>"

instance Show SomePrimitiveVector where
  show _ = "<Data.Vector.Primitive.Vector>"

instance Show SomePrimitiveVectorM where
  show _ = "<Data.Vector.Primitive.MVector>"

instance Show SomeMutableByteArray where
  show _ = "<Data.Primitive.ByteArray.MutableByteArray>"
