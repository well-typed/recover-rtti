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
    go (C_Prim         _) = 1
    go (C_Other        c) = sizeOther c
    go (C_Maybe        c) = 1 + goElems c
    go (C_Either       c) = 1 + goElems c
    go (C_List         c) = 1 + goElems c
    go (C_Ratio        c) = 1 + goElems c
    go (C_Set          c) = 1 + goElems c
    go (C_Map          c) = 1 + goElems c
    go (C_IntMap       c) = 1 + goElems c
    go (C_Sequence     c) = 1 + goElems c
    go (C_Tree         c) = 1 + goElems c
    go (C_HashSet      c) = 1 + goElems c
    go (C_HashMap      c) = 1 + goElems c
    go (C_HM_Array     c) = 1 + goElems c
    go (C_Prim_Array   c) = 1 + goElems c
    go (C_Vector_Boxed c) = 1 + goElems c
    go (C_Tuple        c) = 1 + goElems c

    goElems :: SListI as => Elems o as -> Int
    goElems (Elems cs) = sum . hcollapse $ hmap (K . goElem) cs

    goElem :: Elem o a -> Int
    goElem NoElem   = 0
    goElem (Elem c) = go c
