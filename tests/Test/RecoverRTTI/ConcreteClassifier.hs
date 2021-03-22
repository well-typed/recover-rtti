{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Test.RecoverRTTI.ConcreteClassifier (
    -- * Concrete classifier
    ConcreteClassifier
  , ClassifyUser(..)
    -- * Values
  , Value(..)
    -- * Show
  , canShowUser
  , canShowConcrete
    -- * Size
  , sizeUser
  , sizeConcrete
    -- * Same classifier
  , sameUser
  , sameConcrete
    -- * Equality
  , canCompareUser
  , canCompareConcrete
    -- * Arbitrary
  , arbitraryUser
  , arbitraryConcrete
  ) where

import Data.Kind
import Data.SOP.Dict
import Data.Type.Equality
import Data.Void

import Debug.RecoverRTTI

import Test.QuickCheck

import Test.RecoverRTTI.Classifier.Arbitrary
import Test.RecoverRTTI.Classifier.Equality
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
  C_NonRec   :: MaybeF ClassifyUser a -> ClassifyUser (NonRecursive a)
  C_Rec      :: MaybeF ClassifyUser a -> ClassifyUser (Recursive    a)
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
  Show
-------------------------------------------------------------------------------}

canShowUser :: ClassifyUser a -> Dict Show a
canShowUser = go
  where
    go :: ClassifyUser a -> Dict Show a
    go  C_Simple    = Dict
    go (C_NonRec c) = goMaybeF c
    go (C_Rec    c) = goMaybeF c
    go  C_Unlifted  = Dict

    goMaybeF ::
         (forall x. Show x => Show (f x))
      => MaybeF ClassifyUser a -> Dict Show (f a)
    goMaybeF FNothing  = Dict
    goMaybeF (FJust c) = (\Dict -> Dict) $ canShowConcrete c

canShowConcrete :: ConcreteClassifier a -> Dict Show a
canShowConcrete = canShowClassified_ canShowUser

{-------------------------------------------------------------------------------
  Size of the classifier

  Mostly used for sanity checking the generator
-------------------------------------------------------------------------------}

sizeUser :: ClassifyUser a -> Int
sizeUser = go
  where
    go :: ClassifyUser a -> Int
    go  C_Simple    = 1
    go (C_NonRec c) = 1 + goMaybeF c
    go (C_Rec    c) = 1 + goMaybeF c
    go  C_Unlifted  = 1

    goMaybeF :: MaybeF ClassifyUser a -> Int
    goMaybeF FNothing  = 0
    goMaybeF (FJust c) = sizeConcrete c

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
    go (C_NonRec c) (C_NonRec c') = goMaybeF c c'
    go (C_Rec    c) (C_Rec    c') = goMaybeF c c'
    go  C_Unlifted   C_Unlifted   = Just Refl
    go  _            _            = Nothing

    goMaybeF ::
         MaybeF ClassifyUser x
      -> MaybeF ClassifyUser x'
      -> Maybe (f x :~: f x')
    goMaybeF FNothing  FNothing   = Just Refl
    goMaybeF (FJust x) (FJust x') = (\Refl -> Refl) <$> sameConcrete x x'
    goMaybeF _          _         = Nothing

    _checkAllCases :: ClassifyUser a -> ()
    _checkAllCases = \case
        C_Simple{}   -> ()
        C_NonRec{}   -> ()
        C_Rec{}      -> ()
        C_Unlifted{} -> ()

{-------------------------------------------------------------------------------
  Equality
-------------------------------------------------------------------------------}

canCompareUser :: forall a. ClassifyUser a -> Dict Eq a
canCompareUser = go
  where
    go :: ClassifyUser a -> Dict Eq a
    go  C_Simple    = Dict
    go (C_NonRec c) = goMaybeF c
    go (C_Rec    c) = goMaybeF c
    go  C_Unlifted  = Dict

    goMaybeF ::
         (forall x. Eq x => Eq (f x))
      => MaybeF ClassifyUser a -> Dict Eq (f a)
    goMaybeF FNothing  = Dict
    goMaybeF (FJust c) = (\Dict -> Dict) $ canCompareConcrete c

canCompareConcrete :: ConcreteClassifier a -> Dict Eq a
canCompareConcrete = canCompareClassified_ canCompareUser

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
      => (forall a. MaybeF ClassifyUser a -> ClassifyUser (f a))
      -> f Void
      -> SizedGen (Some (GenJust ConcreteClassifier f))
      -> SizedGen (Some (DepGen ClassifyUser))
    goMaybeF cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf FNothing) (pure nothing))
          [(\(Some a) -> Some (genJust (cf . FJust) a)) <$> just]

arbitraryConcrete :: SizedGen (Some (DepGen ConcreteClassifier))
arbitraryConcrete = arbitraryClassifier_ arbitraryUser
