{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Debug.RecoverRTTI.FlatClosure (
    FlatClosure(..)
  , getBoxedClosureData
    -- * Re-exports
  , Box(..)
  , asBox
  ) where

import Control.Exception (evaluate)
import Control.Monad
import GHC.Exts.Heap (Box(..), asBox)
import qualified GHC.Exts.Heap as H

-- | Flattened form of 'Closure' (with indirection nodes removed)
--
-- We only include the fields of 'Closure' that we are interested in.
--
-- TODO: For functions ('FunClosure', 'PAPClosure') we don't currently include
-- any information at all. We could potentially do better here.
data FlatClosure =
    -- | Constructor application
    ConstrClosure {
        ptrArgs :: [Box]
      , pkg     :: String
      , modl    :: String
      , name    :: String
      }

    -- | Functions
    --
    -- We map 'H.FunClosure', 'H.PAPClosure' and H.BCOClosure' all to this.
  | FunClosure

    -- | Unrecognized closure type
  | OtherClosure H.Closure
  deriving (Show)

getBoxedClosureData :: Box -> IO FlatClosure
getBoxedClosureData b = do
    tryForceBox b
    fromClosure =<< H.getBoxedClosureData b
  where
    fromClosure :: H.Closure -> IO FlatClosure
    fromClosure = \case
        -- Indirections
        --
        -- For background on black holes, see "Implementing Lazy Functional
        -- Languages on Stock Hardware: The Spineless Tagless G-machine", Simon
        -- Peyton Jones, Journal of Functional Programming, July 1992, section
        -- 9.3.3 "Black holes".

        H.BlackholeClosure _ x' -> getBoxedClosureData x'
        H.IndClosure       _ x' -> getBoxedClosureData x'

        -- Constructor application

        H.ConstrClosure{ptrArgs, pkg, modl, name} ->
          return $ ConstrClosure{..}

        -- Functions

        H.FunClosure{} -> return $ FunClosure
        H.PAPClosure{} -> return $ FunClosure
        H.BCOClosure{} -> return $ FunClosure

        -- Other kinds of constructors

        otherClosure ->
          return $ OtherClosure otherClosure

-- | Force the value to WHNF, if possible
--
-- We /cannot/ force the argument until we know what kind of closure we're
-- dealing with. If this is an unlifted closure, forcing it will result in a
-- ghc runtime crash.
tryForceBox :: Box -> IO ()
tryForceBox b@(Box x) = do
    closure <- H.getBoxedClosureData b
    case closure of

      H.APClosure{}       -> void $ evaluate x
      H.ThunkClosure{}    -> void $ evaluate x
      H.SelectorClosure{} -> void $ evaluate x
      _otherwise          -> return ()
