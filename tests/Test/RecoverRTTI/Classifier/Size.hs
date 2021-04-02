{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.RecoverRTTI.Classifier.Size (classifierSize_) where

import Data.SOP

import Debug.RecoverRTTI

{-------------------------------------------------------------------------------
  Size
-------------------------------------------------------------------------------}

classifierSize_ :: forall o.
      (forall a. o a -> Int)
   -> (forall a. Classifier_ o a -> Int)
classifierSize_ sizeOther = go
  where
    go :: Classifier_ o a -> Int
    go (C_Prim            _) = 1
    go (C_Other           c) = sizeOther c
    go (C_Maybe           c) = 1 + goMaybeF     c
    go (C_Either          c) = 1 + goEitherF    c
    go (C_List            c) = 1 + goMaybeF     c
    go (C_Ratio           c) = 1 + go           c
    go (C_Set             c) = 1 + goMaybeF     c
    go (C_Map             c) = 1 + goMaybePairF c
    go (C_IntMap          c) = 1 + goMaybeF     c
    go (C_Sequence        c) = 1 + goMaybeF     c
    go (C_Tree            c) = 1 + go           c
    go (C_HashSet         c) = 1 + go           c
    go (C_HashMap         c) = 1 + goMaybePairF c
    go (C_HM_Array        c) = 1 + goMaybeF     c
    go (C_Prim_Array      c) = 1 + goMaybeF     c
    go (C_Vector_Boxed    c) = 1 + goMaybeF     c
    go (C_Vector_Unboxed  c) = 1 + goClosedF    c
    go (C_Vector_UnboxedM c) = 1 + goClosedF    c
    go (C_Tuple           c) = 1 + goTuple      c

    goClosedF :: Classifier_ C a -> Int
    goClosedF = classifierSize_ noOtherTypes
      where
        noOtherTypes :: C a -> Int
        noOtherTypes x = case x of {}

    goMaybeF :: MaybeF o a -> Int
    goMaybeF FNothing  = 0
    goMaybeF (FJust c) = go c

    goEitherF :: EitherF o a b -> Int
    goEitherF (FLeft  c) = go c
    goEitherF (FRight c) = go c

    goMaybePairF :: MaybePairF o a b -> Int
    goMaybePairF FNothingPair     = 0
    goMaybePairF (FJustPair c c') = go c + go c'

    goTuple :: SListI xs => Classifiers o xs -> Int
    goTuple = sum . hcollapse . hmap (K . go) . getClassifiers
