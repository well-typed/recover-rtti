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

import GHC.Exts.Heap (Box(..), asBox)
import qualified GHC.Exts.Heap as H

-- | Flattened form of 'Closure' (with indirection nodes removed)
--
-- We only include the fields of 'Closure' that we are interested in.
--
-- TODO: For functions ('FunClosure', 'PAPClosure') we don't currently include
-- any information at all. We could potentially do better here.
data FlatClosure =
    ConstrClosure {
        ptrArgs :: [Box]
      , pkg     :: String
      , modl    :: String
      , name    :: String
      }
  | FunClosure
  | PAPClosure
  | OtherClosure H.Closure
  deriving (Show)

getBoxedClosureData :: Box -> IO FlatClosure
-- It is important that we force the value to WHNF /before/ getting the closure
getBoxedClosureData b@(Box !_) = --
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
        H.SelectorClosure  _ x' -> getBoxedClosureData x'

        -- Cases we're actually interested in

        H.ConstrClosure{ptrArgs, pkg, modl, name} ->
          return $ ConstrClosure{..}
        H.FunClosure{} ->
          return $ FunClosure
        H.PAPClosure{} ->
          return $ PAPClosure

        -- Other kinds of constructors

        otherClosure ->
          return $ OtherClosure otherClosure
