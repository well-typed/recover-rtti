{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.RecoverRTTI.ConcreteClassifier (
    -- * Concrete classifier
    ConcreteClassifier
  , ClassifyUser(..)
    -- * Values
  , Value(..)
    -- * Constraints
  , canShowConcrete
  , canCompareConcrete
    -- * Size
  , sizeUser
  , sizeConcrete
    -- * Same classifier
  , sameUser
  , sameConcrete
    -- * Equality
    -- * Arbitrary
  , arbitraryUser
  , arbitraryConcrete
  ) where

import Data.Kind
import Data.SOP
import Data.SOP.Dict
import Data.Type.Equality
import Data.Void

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Classify

import Test.QuickCheck

import Test.RecoverRTTI.Classifier.Arbitrary
import Test.RecoverRTTI.Classifier.Equality ()
import Test.RecoverRTTI.Classifier.Size
import Test.RecoverRTTI.QuickCheck.DepGen
import Test.RecoverRTTI.QuickCheck.Sized (SizedGen)
import Test.RecoverRTTI.UserDefined

import qualified Test.RecoverRTTI.QuickCheck.Sized as SG

{-------------------------------------------------------------------------------
  Concrete classifier

  The difference between the " concrete " classifier and the 'Classifier' from
  the main library is that the former has explicit cases for user-defined types,
  and the latter doesn't (merely classifying them as 'UserDefined').

  In "Test.RecoverRRTI.Staged" we show that we can do staged inference,
  using 'classify' repeatedly to recover /all/ (concrete) type information
  from the type information returned by 'classify' (/if/ we have full
  information about which user-defined types we're interested in).
-------------------------------------------------------------------------------}

type ConcreteClassifier = Classifier_ ClassifyUser

data ClassifyUser (a :: Type) where
  C_Simple   :: ClassifyUser SimpleType
  C_NonRec   :: Elems ClassifyUser '[a] -> ClassifyUser (NonRecursive a)
  C_Rec      :: Elems ClassifyUser '[a] -> ClassifyUser (Recursive    a)
  C_Unlifted :: ClassifyUser ContainsUnlifted

deriving instance Show (ClassifyUser a)

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Like 'Classified', but using 'ConcreteClassifier'
--
-- For convenience, we also include some constraints here, even though they
-- are in fact derivable from the classifier
data Value a where
   Value :: (Show a, Eq a) => ConcreteClassifier a -> a -> Value a

deriving instance Show (Value a)
deriving instance Show (Some Value)

instance Arbitrary (Some Value) where
  arbitrary = do
      -- We don't want to generate large classifiers
      Some (DepGen cc gen) <- SG.run 10 arbitraryConcrete

      -- For the values however we want to be able to generate larger trees
      Some . Value cc <$> SG.run 1000 gen

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

class (
    c SimpleType
  , forall a. c a => c (NonRecursive a)
  , forall a. c a => c (Recursive    a)
  , c ContainsUnlifted
  ) => UserSatisfies c

instance (
    c SimpleType
  , forall a. c a => c (NonRecursive a)
  , forall a. c a => c (Recursive    a)
  , c ContainsUnlifted
  ) => UserSatisfies c

userSatisfies :: forall c.
     (ClassifiedSatisfies c, c Void, UserSatisfies c)
  => (forall a. ClassifyUser a -> Dict c a)
userSatisfies = go
  where
    go :: ClassifyUser a -> Dict c a
    go  C_Simple    = Dict
    go (C_NonRec c) = goElems c $ Dict
    go (C_Rec    c) = goElems c $ Dict
    go  C_Unlifted  = Dict

    goElems :: SListI as => Elems ClassifyUser as -> (All c as => r) -> r
    goElems (Elems cs) k = case all_NP (hmap goElem cs) of Dict -> k

    goElem :: Elem ClassifyUser a -> Dict c a
    goElem (Elem c) = concreteSatisfies c
    goElem NoElem   = Dict

concreteSatisfies ::
     (ClassifiedSatisfies c, c Void, UserSatisfies c)
  => ConcreteClassifier a -> Dict c a
concreteSatisfies = classifiedSatisfies userSatisfies

canShowConcrete :: ConcreteClassifier a -> Dict Show a
canShowConcrete = concreteSatisfies

canCompareConcrete :: ConcreteClassifier a -> Dict Eq a
canCompareConcrete = concreteSatisfies

{-------------------------------------------------------------------------------
  Size of the classifier

  Mostly used for sanity checking the generator
-------------------------------------------------------------------------------}

sizeUser :: ClassifyUser a -> Int
sizeUser = go
  where
    go :: ClassifyUser a -> Int
    go  C_Simple    = 1
    go (C_NonRec c) = 1 + goElems c
    go (C_Rec    c) = 1 + goElems c
    go  C_Unlifted  = 1

    goElems :: SListI as => Elems ClassifyUser as -> Int
    goElems (Elems cs) = sum . hcollapse $ hmap (K . goElem) cs

    goElem :: Elem ClassifyUser a -> Int
    goElem NoElem   = 0
    goElem (Elem c) = sizeConcrete c

sizeConcrete :: ConcreteClassifier a -> Int
sizeConcrete = classifierSize_ sizeUser

{-------------------------------------------------------------------------------
  Same classifier
-------------------------------------------------------------------------------}

-- | Check that two classifiers are the same
sameConcrete ::
     ConcreteClassifier a
  -> ConcreteClassifier b
  -> Maybe (a :~: b)
sameConcrete = sameClassifier_ sameUser

sameUser :: ClassifyUser a -> ClassifyUser b -> Maybe (a :~: b)
sameUser = go
  where
    go :: ClassifyUser a -> ClassifyUser b -> Maybe (a :~: b)
    go  C_Simple     C_Simple     = Just Refl
    go (C_NonRec c) (C_NonRec c') = sameElems sameUser c c' $ Refl
    go (C_Rec    c) (C_Rec    c') = sameElems sameUser c c' $ Refl
    go  C_Unlifted   C_Unlifted   = Just Refl
    go  _            _            = Nothing

    _checkAllCases :: ClassifyUser a -> ()
    _checkAllCases = \case
        C_Simple{}   -> ()
        C_NonRec{}   -> ()
        C_Rec{}      -> ()
        C_Unlifted{} -> ()

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

arbitraryUser :: SizedGen (Some (DepGen ClassifyUser))
arbitraryUser = SG.leafOrStep leaf compound
  where
    leaf :: Gen (Some (DepGen ClassifyUser))
    leaf = oneof [
          -- SimpleType
          pure . Some $ arbitraryDepGen C_Simple

          -- ContainsUnlifted
        , pure . Some $ arbitraryDepGen C_Unlifted
        ]

    compound :: [SizedGen (Some (DepGen ClassifyUser))]
    compound = [
          -- NonRecursive
          goMaybeF C_NonRec (NR1 1234)
            (mapSome (GenJust (fmap (NR2 True))) <$> arbitraryConcrete)

          -- Recursive
        , goMaybeF C_Rec RNil
            (mapSome (GenJust (SG.genListLike recursiveFromList)) <$> arbitraryConcrete)
        ]

    goMaybeF ::
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall a. Elems ClassifyUser '[a] -> ClassifyUser (f a))
      -> f Void
      -> SizedGen (Some (GenJust ConcreteClassifier f))
      -> SizedGen (Some (DepGen ClassifyUser))
    goMaybeF cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf ElemU) (pure nothing))
          [(\(Some a) -> Some (genJust (cf . ElemK) a)) <$> just]

arbitraryConcrete :: SizedGen (Some (DepGen ConcreteClassifier))
arbitraryConcrete = arbitraryClassifier_ arbitraryUser
