{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.RecoverRTTI.Classifier.Arbitrary (arbitraryClassifier_) where

import Data.Bifunctor
import Data.Kind
import Data.SOP
import Data.Tree (Tree)
import Data.Void
import GHC.Real (Ratio((:%)))

import qualified Data.HashMap.Internal.Array as HashMap.Array
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.HashSet                as HashSet
import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import qualified Data.Primitive.Array        as Prim.Array
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Tree                   as Tree
import qualified Data.Vector                 as Vector.Boxed

import Debug.RecoverRTTI
import Debug.RecoverRTTI.Classify

import Test.QuickCheck

import Test.RecoverRTTI.Classifier.Equality ()
import Test.RecoverRTTI.Prim
import Test.RecoverRTTI.QuickCheck.DepGen
import Test.RecoverRTTI.QuickCheck.Sized (SizedGen)

import qualified Test.RecoverRTTI.QuickCheck.Sized as SG

{-------------------------------------------------------------------------------
  Generate arbitiary classifiers
-------------------------------------------------------------------------------}

-- | Generated arbitrary classifier along with a generator for that value
--
-- NOTE: The " size " here refers to the size of the /classifier/. Along with
-- the classifier we construct a generator for values of the corresponding
-- type; that generator in turn has its own (independent) size parameter.
arbitraryClassifier_ :: forall c o.
     (c ~ Classifier_ o)
  => SizedGen (Some (DepGen o)) -> SizedGen (Some (DepGen c))
arbitraryClassifier_  genOther = go
  where
    go :: SizedGen (Some (DepGen c))
    go = SG.leafOrStep leaf compound

    -- Leaves of the tree (values with no recursion).
    --
    -- We will fail to generate a leaf when the size reaches 0; this ensures
    -- termination.
    leaf :: Gen (Some (DepGen c))
    leaf = do
        Some c <- arbitraryPrimClassifier
        return $ Some $ primDepGen c

    -- Compound
    --
    -- We deduct one from the size for the outer-most constructor
    --
    -- For most types we generate arbitrary subtypes, but for some types we
    -- must pick subtypes satisfying a certain constraint (e.g., @Ord@ for
    -- @Set@); for such types we just pick a single example.
    compound :: [SizedGen (Some (DepGen c))]
    compound = [
          -- We include " other " in the compound list, so that we are sure
          -- to subtract one from the size
          (\(Some (DepGen c gen)) -> Some (DepGen (C_Other c) gen)) <$> genOther

        , goMaybeF C_Maybe Nothing
            (mapSome (GenJust (fmap Just)) <$> go)

        , goEitherF C_Either
            (mapSome (GenLeft  (fmap Left))  <$> go)
            (mapSome (GenRight (fmap Right)) <$> go)

          -- @[Char]@ is classified as @String@
        , let notChar (Some (DepGen (C_Prim C_Char) _)) = False
              notChar _otherwise = True in
          goMaybeF C_List []
            (mapSome (GenJust (SG.genListLike id)) <$> (go `SG.suchThat` notChar))

        , goF C_Ratio $ pure . Some $ GenJust {
              justGen  = \g -> uncurry (:%) <$> SG.divvyPair g g
            , justElem = primDepGen C_Int
            }

        , goMaybeF C_Set Set.empty $ pure . Some $ GenJust {
              justGen  = SG.genListLike Set.fromList
            , justElem = primDepGen C_Int
            }

        , goMaybePairF C_Map Map.empty
            ((\(Some genElem) -> Some $ GenPair {
                pairGen = SG.genMapLike Map.fromList
              , pairFst = primDepGen C_Int
              , pairSnd = genElem
              }) <$> go)

        , goMaybeF C_IntMap IntMap.empty
            ((\(Some genElem) -> Some $ GenJust {
                justGen  = SG.genMapLike IntMap.fromList SG.arbitrary
              , justElem = genElem
              }) <$> go)

        , goMaybeF C_Sequence Seq.empty
            (mapSome (GenJust (SG.genListLike Seq.fromList)) <$> go)

        , goF C_Tree
            (mapSome (GenJust (SG.genListLike mkSomeTree)) <$> go)

        , goF C_HashSet $ pure . Some $ GenJust {
              justGen  = SG.genListLike HashSet.fromList
            , justElem = primDepGen C_Int
            }

          -- @HashMap a ()@ is classified as a @HashSet@ instead
        , let notUnit (Some (DepGen (C_Prim C_Unit) _)) = False
              notUnit _otherwise = True in
          goMaybePairF C_HashMap HashMap.empty
            ((\(Some genElem) -> Some $ GenPair {
                pairGen = SG.genMapLike HashMap.fromList
              , pairFst = primDepGen C_Int
              , pairSnd = genElem
              }) <$> (go `SG.suchThat` notUnit))

        , let mkArray xs = HashMap.Array.fromList (length xs) xs in
          goMaybeF C_HM_Array (mkArray [])
            (mapSome (GenJust (SG.genListLike mkArray)) <$> go)

        , goMaybeF C_Prim_Array (Prim.Array.arrayFromList [])
            (mapSome (GenJust (SG.genListLike Prim.Array.arrayFromList)) <$> go)

        , goMaybeF C_Vector_Boxed Vector.Boxed.empty
            (mapSome (GenJust (SG.genListLike Vector.Boxed.fromList)) <$> go)

        , goTuple
        ]

    goF :: forall f.
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall x. Elems o '[x] -> c (f x))
      -> SizedGen (Some (GenJust c f))
      -> SizedGen (Some (DepGen c))
    goF cf = fmap (\(Some a) -> Some (genJust (cf . ElemK) a))

    goMaybeF :: forall f.
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall x. Elems o '[x] -> c (f x))
      -> f Void
      -> SizedGen (Some (GenJust c f))
      -> SizedGen (Some (DepGen c))
    goMaybeF cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf ElemU) (pure nothing))
          [(\(Some a) -> Some (genJust (cf . ElemK) a)) <$> just]

    goEitherF :: forall f.
         ( forall x y. (Show x, Show y) => Show (f x y)
         , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
         )
      => (forall x y. Elems o '[x, y] -> c (f x y))
      -> SizedGen (Some (GenLeft  c f))
      -> SizedGen (Some (GenRight c f))
      -> SizedGen (Some (DepGen c))
    goEitherF cf left right =
        SG.oneofStepped [
            (\(Some a) -> Some (genLeft  (cf . ElemKU)  a)) <$> left
          , (\(Some b) -> Some (genRight (cf . ElemUK) b)) <$> right
          ]

    goMaybePairF :: forall (f :: Type -> Type -> Type).
         ( forall x y. (Show x, Show y) => Show (f x y)
         , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
         )
      => (forall x y. Elems o '[x, y] -> c (f x y))
      -> f Void Void
      -> SizedGen (Some (GenPair c f))
      -> SizedGen (Some (DepGen c))
    goMaybePairF cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf ElemUU) (pure nothing))
          [(\(Some ab@GenPair{}) -> Some (genPair (cf . uncurry ElemKK) ab)) <$> just]

    goTuple :: SizedGen (Some (DepGen c))
    goTuple =
        (\(Some (SG.ValidTuple t)) -> Some (lift t)) <$> SG.genTuple go
      where
        lift :: (SListI xs, IsValidSize (Length xs))
          => NP (DepGen (Classifier_ o)) xs
          -> DepGen (Classifier_ o) (WrappedTuple xs)
        lift t = genNP (C_Tuple . Elems . hmap Elem) $ GenNP {
              npGen  = fmap tupleFromNP . hsequence
            , npElem = t
            }

    _checkAllCases :: Classifier_ o a -> ()
    _checkAllCases = \case
        -- Primitive and user-defined
        C_Prim{}  -> ()
        C_Other{} -> ()

        -- Compound
        C_Maybe{}        -> ()
        C_Either{}       -> ()
        C_List{}         -> ()
        C_Ratio{}        -> ()
        C_Set{}          -> ()
        C_Map{}          -> ()
        C_IntMap{}       -> ()
        C_Sequence{}     -> ()
        C_Tree{}         -> ()
        C_HashSet{}      -> ()
        C_HashMap{}      -> ()
        C_HM_Array{}     -> ()
        C_Prim_Array{}   -> ()
        C_Vector_Boxed{} -> ()
        C_Tuple{}        -> ()

{-------------------------------------------------------------------------------
  Auxiliary tree functions
-------------------------------------------------------------------------------}

mkSomeTree :: [a] -> Tree a
mkSomeTree []       = error "mkSomeTree: empty"
mkSomeTree [x]      = Tree.Node x []
mkSomeTree [x, y]   = Tree.Node x [Tree.Node y []]
mkSomeTree (x : xs) =
    let (left, right) = split xs
    in Tree.Node x [mkSomeTree left, mkSomeTree right]

-- | Split list into halves
--
-- If the input has at least two elements, neither list will be empty
--
-- > split "abcde" == ("ace","bd")
split :: [a] -> ([a], [a])
split []     = ([], [])
split (x:xs) = first (x:) $ splot xs

-- | Auxiliary to 'split'
splot :: [a] -> ([a], [a])
splot []     = ([], [])
splot (x:xs) = second (x:) $ split xs
