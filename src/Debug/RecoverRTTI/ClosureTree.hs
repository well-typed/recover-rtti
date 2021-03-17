{-# LANGUAGE BangPatterns #-}

module Debug.RecoverRTTI.ClosureTree (
    showClosureTree
  ) where

import Data.List
import GHC.Exts.Heap

-- | Show closure tree up to the given depth
--
-- Used only for internal debugging
showClosureTree :: Int -> a -> IO String
showClosureTree = \d -> go d 0 . asBox
  where
    go :: Int -> Int -> Box -> IO String
    go 0 _ _ = return ""
    go d i x = do
        closure <- getBoxedClosureData x
        render closure <$> mapM (go (d - 1) (i + 2)) (allClosures closure)
      where
        render :: Closure -> [String] -> String
        render closure nested = intercalate "\n" $
              (replicate i ' ' ++ show closure)
            : nested
