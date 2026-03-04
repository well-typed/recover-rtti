{-# LANGUAGE CPP #-}

module Test.RecoverRTTI.ConcreteClassifier.Arbitrary (
    arbitraryConcrete
  ) where

import Data.Kind
import Data.SOP
import Data.SOP.Dict
import Data.Tree (Tree)
import Data.Vector qualified as Vector.Boxed
import Data.Void
import Test.QuickCheck (Gen)
import Test.QuickCheck qualified as QC

import Data.Bifunctor
import Data.HashMap.Internal.Array qualified as HashMap.Array
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Tree qualified as Tree
import GHC.Real (Ratio((:%)))

#if MIN_VERSION_base(4,17,0)
import GHC.IsList qualified as IsList
#else
import GHC.Exts qualified as IsList (fromList)
#endif

import Debug.RecoverRTTI

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.Prim
import Test.RecoverRTTI.QuickCheck.DepGen
import Test.RecoverRTTI.QuickCheck.Sized (SizedGen)
import Test.RecoverRTTI.QuickCheck.Sized qualified as SG
import Test.RecoverRTTI.UserDefined

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

primDepGen :: PrimClassifier a -> DepGen Concrete a
primDepGen c =
    case (primSatisfiesArbitrary c, canShowPrim c, canComparePrim c) of
      (Dict, Dict, Dict) -> DepGen (CC_Prim c) $ unwrap <$> SG.arbitrary

{-------------------------------------------------------------------------------
  User-defined types
-------------------------------------------------------------------------------}

arbitraryUser :: SizedGen (Some (DepGen ConcreteUser))
arbitraryUser = SG.leafOrStep leaf compound
  where
    leaf :: Gen (Some (DepGen ConcreteUser))
    leaf = QC.oneof [
          -- SimpleType
          pure . Some $ arbitraryDepGen CC_Simple

          -- ContainsUnlifted
        , pure . Some $ arbitraryDepGen CC_Unlifted
        ]

    compound :: [SizedGen (Some (DepGen ConcreteUser))]
    compound = [
          -- NonRecursive
          go_U_K CC_NonRec (NR1 1234)
            (mapSome (GenK (fmap (NR2 True))) <$> arbitraryConcrete)

          -- Recursive
        , go_U_K CC_Rec RNil
            (mapSome (GenK (SG.genListLike recursiveFromList)) <$> arbitraryConcrete)
        ]

    go_U_K ::
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall a. Concrete a -> ConcreteUser (f a))
      -> (forall a. f a)
      -> SizedGen (Some (GenK Concrete f))
      -> SizedGen (Some (DepGen ConcreteUser))
    go_U_K cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf CC_Void) (pure nothing))
          [(\(Some a) -> Some (genJust cf  a)) <$> just]

{-------------------------------------------------------------------------------
  Generate arbitiary classifiers
-------------------------------------------------------------------------------}

-- | Generated arbitrary classifier along with a generator for that value
--
-- NOTE: The " size " here refers to the size of the /classifier/. Along with
-- the classifier we construct a generator for values of the corresponding
-- type; that generator in turn has its own (independent) size parameter.
arbitraryConcrete :: SizedGen (Some (DepGen Concrete))
arbitraryConcrete  = go
  where
    go :: SizedGen (Some (DepGen Concrete))
    go = SG.leafOrStep leaf compound

    -- Leaves of the tree (values with no recursion).
    --
    -- We will fail to generate a leaf when the size reaches 0; this ensures
    -- termination.
    leaf :: Gen (Some (DepGen Concrete))
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
    compound :: [SizedGen (Some (DepGen Concrete))]
    compound = [
          (\(Some (DepGen c gen)) -> Some(DepGen (CC_Other c) gen)) <$> arbitraryUser

        , go_U_K CC_Maybe Nothing
            (mapSome (GenK (fmap Just)) <$> go)

        , go_KU_UK CC_Either
            (mapSome (GenKU  (fmap Left))  <$> go)
            (mapSome (GenUK (fmap Right)) <$> go)

          -- @[Char]@ is classified as @String@
        , let notChar (Some (DepGen (CC_Prim C_Char) _)) = False
              notChar _otherwise = True in
          go_U_K CC_List []
            (mapSome (GenK (SG.genListLike id)) <$> (go `SG.suchThat` notChar))

        , go_K CC_Ratio $ pure . Some $ GenK {
              justGen  = \g -> uncurry (:%) <$> SG.divvyPair g g
            , justElem = primDepGen C_Int
            }

        , go_U_K CC_Set Set.empty $ pure . Some $ GenK {
              justGen  = SG.genListLike Set.fromList
            , justElem = primDepGen C_Int
            }

        , go_UU_KK CC_Map Map.empty
            ((\(Some genElem) -> Some $ GenKK {
                pairGen = SG.genMapLike Map.fromList
              , pairFst = primDepGen C_Int
              , pairSnd = genElem
              }) <$> go)

        , go_U_K CC_IntMap IntMap.empty
            ((\(Some genElem) -> Some $ GenK {
                justGen  = SG.genMapLike IntMap.fromList SG.arbitrary
              , justElem = genElem
              }) <$> go)

        , go_U_K CC_Sequence Seq.empty
            (mapSome (GenK (SG.genListLike Seq.fromList)) <$> go)

        , go_K CC_Tree
            (mapSome (GenK (SG.genListLike mkSomeTree)) <$> go)

        , go_K CC_HashSet $ pure . Some $ GenK {
              justGen  = SG.genListLike HashSet.fromList
            , justElem = primDepGen C_Int
            }

          -- @HashMap a ()@ is classified as a @HashSet@ instead
        , let notUnit (Some (DepGen (CC_Prim C_Unit) _)) = False
              notUnit _otherwise = True in
          go_UU_KK CC_HashMap HashMap.empty
            ((\(Some genElem) -> Some $ GenKK {
                pairGen = SG.genMapLike HashMap.fromList
              , pairFst = primDepGen C_Int
              , pairSnd = genElem
              }) <$> (go `SG.suchThat` notUnit))

        , let mkArray xs = HashMap.Array.fromList (length xs) xs in
          go_U_K CC_HM_Array (mkArray [])
            (mapSome (GenK (SG.genListLike mkArray)) <$> go)

        , go_U_K CC_Prim_Array (IsList.fromList [])
            (mapSome (GenK (SG.genListLike IsList.fromList)) <$> go)

        , go_U_K CC_Vector_Boxed Vector.Boxed.empty
            (mapSome (GenK (SG.genListLike Vector.Boxed.fromList)) <$> go)

        , goTuple
        ]

    go_K :: forall f.
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall x. Concrete x -> Concrete (f x))
      -> SizedGen (Some (GenK Concrete f))
      -> SizedGen (Some (DepGen Concrete))
    go_K cf = fmap (\(Some a) -> Some (genJust cf a))

    go_U_K :: forall f.
         ( forall x. Show x => Show (f x)
         , forall x. Eq   x => Eq   (f x)
         )
      => (forall x. Concrete x -> Concrete (f x))
      -> f Void
      -> SizedGen (Some (GenK Concrete f))
      -> SizedGen (Some (DepGen Concrete))
    go_U_K cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf CC_Void) (pure nothing))
          [(\(Some a) -> Some (genJust cf a)) <$> just]

    go_KU_UK :: forall f.
         ( forall x y. (Show x, Show y) => Show (f x y)
         , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
         )
      => (forall x y. Concrete x -> Concrete y -> Concrete (f x y))
      -> SizedGen (Some (GenKU Concrete f))
      -> SizedGen (Some (GenUK Concrete f))
      -> SizedGen (Some (DepGen Concrete))
    go_KU_UK cf left right =
        SG.oneofStepped [
            (\(Some a) -> Some (genLeft  (\cc -> cf cc CC_Void) a)) <$> left
          , (\(Some b) -> Some (genRight (\cc -> cf CC_Void cc) b)) <$> right
          ]

    go_UU_KK :: forall (f :: Type -> Type -> Type).
         ( forall x y. (Show x, Show y) => Show (f x y)
         , forall x y. (Eq   x, Eq   y) => Eq   (f x y)
         )
      => (forall x y. Concrete x -> Concrete y -> Concrete (f x y))
      -> (forall x y. f x y)
      -> SizedGen (Some (GenKK Concrete f))
      -> SizedGen (Some (DepGen Concrete))
    go_UU_KK cf nothing just =
        SG.leafOrStep
          (pure $ Some $ DepGen (cf CC_Void CC_Void) (pure nothing))
          [(\(Some ab@GenKK{}) -> Some (genPair (uncurry cf) ab)) <$> just]

    goTuple :: SizedGen (Some (DepGen Concrete))
    goTuple =
        (\(Some (SG.ValidTuple t)) -> Some (lift t)) <$> SG.genTuple go
      where
        lift :: (SListI xs, IsValidSize (Length xs))
          => NP (DepGen Concrete) xs
          -> DepGen Concrete (WrappedTuple xs)
        lift t = genNP (CC_Tuple . Concretes) $ GenNP {
              npGen  = fmap tupleFromNP . hsequence
            , npElem = t
            }

    _checkAllCases :: Classifier_ o a -> ()
    _checkAllCases = \case
        -- Primitive and user-defined
        C_Prim{}  -> ()
        C_Other{} -> ()

        -- Compound
        C_Either{}       -> ()
        C_HashMap{}      -> ()
        C_HashSet{}      -> ()
        C_HM_Array{}     -> ()
        C_IntMap{}       -> ()
        C_List{}         -> ()
        C_Map{}          -> ()
        C_Maybe{}        -> ()
        C_Prim_Array{}   -> ()
        C_Ratio{}        -> ()
        C_Sequence{}     -> ()
        C_Set{}          -> ()
        C_Tree{}         -> ()
        C_Tuple{}        -> ()
        C_Vector_Boxed{} -> ()

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
