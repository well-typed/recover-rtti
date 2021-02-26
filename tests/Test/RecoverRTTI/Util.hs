{-# LANGUAGE ScopedTypeVariables #-}

module Test.RecoverRTTI.Util (
    -- * Monadic functions
    anyM
    -- * Working with closures
  , containsBlackholes
  ) where

import GHC.Exts.Heap
import System.IO.Unsafe (unsafePerformIO)

import Data.Foldable as Foldable

{-------------------------------------------------------------------------------
  Monadic functions
-------------------------------------------------------------------------------}

anyM :: forall t a m. (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = go . Foldable.toList
  where
    go :: [a] -> m Bool
    go []     = return False
    go (x:xs) = do b <- f x
                   if b then return True
                        else go xs

{-------------------------------------------------------------------------------
  Working with closures
-------------------------------------------------------------------------------}

containsBlackholes :: a -> Bool
containsBlackholes =
    unsafePerformIO . go . asBox
  where
    go :: Box -> IO Bool
    go b = do
        closure <- getBoxedClosureData b
        case closure of
          BlackholeClosure{} -> return True
          _otherwise         -> anyM go (allClosures closure)
