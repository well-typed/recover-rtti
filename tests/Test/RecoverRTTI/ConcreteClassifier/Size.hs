module Test.RecoverRTTI.ConcreteClassifier.Size (
    sizeConcrete
  , sizeUser
  ) where

import Data.SOP

import Test.RecoverRTTI.ConcreteClassifier

{-------------------------------------------------------------------------------
  Size of the classifier

  Mostly used for sanity checking the generator
-------------------------------------------------------------------------------}

sizeConcrete :: Concrete a -> Int
sizeConcrete = go
  where
    go :: Concrete a -> Int
    go (CC_Prim         _    ) = 1
    go (CC_Other        c    ) = sizeUser c
    go CC_Void                 = 1

    go (CC_HashSet      c1   ) = 1 + go c1
    go (CC_IntMap       c1   ) = 1 + go c1
    go (CC_Maybe        c1   ) = 1 + go c1
    go (CC_Ratio        c1   ) = 1 + go c1
    go (CC_Set          c1   ) = 1 + go c1
    go (CC_Tree         c1   ) = 1 + go c1

    go (CC_HM_Array     c1   ) = 1 + go c1
    go (CC_List         c1   ) = 1 + go c1
    go (CC_Prim_Array   c1   ) = 1 + go c1
    go (CC_Sequence     c1   ) = 1 + go c1
    go (CC_Vector_Boxed c1   ) = 1 + go c1

    go (CC_Either       c1 c2) = 1 + go c1 + go c2
    go (CC_HashMap      c1 c2) = 1 + go c1 + go c2
    go (CC_Map          c1 c2) = 1 + go c1 + go c2

    go (CC_Tuple        cs   ) = 1 + goNP cs

    goNP :: SListI as => Concretes as -> Int
    goNP (Concretes cs) = sum . hcollapse $ hmap (K . go) cs

sizeUser :: ConcreteUser a -> Int
sizeUser = go
  where
    go :: ConcreteUser a -> Int
    go  CC_Simple    = 1
    go (CC_NonRec c) = 1 + sizeConcrete c
    go (CC_Rec    c) = 1 + sizeConcrete c
    go  CC_Unlifted  = 1
