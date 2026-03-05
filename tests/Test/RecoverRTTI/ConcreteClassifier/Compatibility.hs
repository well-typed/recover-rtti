module Test.RecoverRTTI.ConcreteClassifier.Compatibility (
    Compatible(..)
  , CompatibleNP(..)
  , compatibleClassifier
  ) where

import Data.SOP.NP
import Data.Type.Equality

import Debug.RecoverRTTI

import Test.RecoverRTTI.ConcreteClassifier
import Test.RecoverRTTI.Staged
import Test.RecoverRTTI.UserDefined

data Compatible a b where
  CompatibleSame     :: Compatible a a
  CompatibleDeferred :: Compatible a Deferred
  CompatibleF1       :: Compatible a b -> Compatible (f a) (f b)
  CompatibleF2       :: Compatible a b
                     -> Compatible a' b'
                     -> Compatible (f a a') (f b b')
  CompatibleFn       :: CompatibleNP as bs -> Compatible (f as) (f bs)

data CompatibleNP as bs where
  CompatibleNil :: CompatibleNP '[] '[]
  CompatibleNP  :: Compatible a b
                -> CompatibleNP as bs
                -> CompatibleNP (a ': as) (b ': bs)

-- | Is the inferred classifier compatible with the generated classifier?
compatibleClassifier :: Concrete a -> UserClassifier b -> Maybe (Compatible a b)
compatibleClassifier = \concrete inferred ->
    case (concrete, inferred) of
      (CC_Prim  c, C_Prim  c') -> compatibleRefl <$> samePrim c c'
      (CC_Other c, C_Other c') -> compatibleUser c c'

      (CC_Maybe{}        , C_Maybe          ) -> Just deferredF1
      (CC_Ratio{}        , C_Ratio          ) -> Just deferredF1
      (CC_Set{}          , C_Set            ) -> Just deferredF1
      (CC_IntMap{}       , C_IntMap         ) -> Just deferredF1
      (CC_Tree{}         , C_Tree           ) -> Just deferredF1
      (CC_HashSet{}      , C_HashSet        ) -> Just deferredF1

      (CC_Sequence     c , C_Sequence     c') -> listElem c c'
      (CC_List         c , C_List         c') -> listElem c c'
      (CC_HM_Array     c , C_HM_Array     c') -> listElem c c'
      (CC_Vector_Boxed c , C_Vector_Boxed c') -> listElem c c'
      (CC_Prim_Array   c , C_Prim_Array   c') -> listElem c c'

      (CC_Either{}       , C_Either         ) -> Just deferredF2
      (CC_HashMap{}      , C_HashMap        ) -> Just deferredF2
      (CC_Map{}          , C_Map            ) -> Just deferredF2

      (CC_Tuple cs       , C_Tuple cs'      ) -> compatibleTuple cs cs'

      _otherwise -> Nothing
  where
    listElem ::
         Concrete a
      -> ClassifyListElem b
      -> Maybe (Compatible (f a) (f b))
    listElem = \concrete inferred ->
        case (concrete, inferred) of
          (_anything      , C_List_Deferred) -> Just $ CompatibleF1 CompatibleDeferred
          (CC_Prim C_Char , C_List_Char    ) -> Just $ CompatibleF1 CompatibleSame
          _otherwise                         -> Nothing

    _checkedAllCases :: Concrete a -> ()
    _checkedAllCases = \case
        CC_Either{}       -> ()
        CC_HashMap{}      -> ()
        CC_HashSet{}      -> ()
        CC_HM_Array{}     -> ()
        CC_IntMap{}       -> ()
        CC_List{}         -> ()
        CC_Map{}          -> ()
        CC_Maybe{}        -> ()
        CC_Other{}        -> ()
        CC_Prim_Array{}   -> ()
        CC_Prim{}         -> ()
        CC_Ratio{}        -> ()
        CC_Sequence{}     -> ()
        CC_Set{}          -> ()
        CC_Tree{}         -> ()
        CC_Tuple{}        -> ()
        CC_Vector_Boxed{} -> ()
        CC_Void{}         -> ()

compatibleUser :: ConcreteUser a -> ClassifyUser b -> Maybe (Compatible a b)
compatibleUser = \concrete inferred ->
    case (concrete, inferred) of
      (CC_Simple   , C_Simple  ) -> Just CompatibleSame
      (CC_NonRec{} , C_NonRec  ) -> Just deferredF1
      (CC_Rec{}    , C_Rec     ) -> Just deferredF1
      (CC_Unlifted , C_Unlifted) -> Just CompatibleSame
      _otherwise                 -> Nothing
  where
    _checkedAllCases :: ConcreteUser a -> ()
    _checkedAllCases = \case
         CC_Simple   -> ()
         CC_NonRec{} -> ()
         CC_Rec{}    -> ()
         CC_Unlifted -> ()

compatibleTuple ::
     Concretes as
  -> Classifiers_ ClassifyUser bs
  -> Maybe (Compatible (WrappedTuple as) (WrappedTuple bs))
compatibleTuple = \(Concretes concrete) (Classifiers_ inferred) ->
    CompatibleFn <$> go concrete inferred
  where
    go ::
         NP Concrete as
      -> NP UserClassifier bs
      -> Maybe (CompatibleNP as bs)
    go Nil       Nil       = Just CompatibleNil
    go Nil       (_ :* _)  = Nothing
    go (_ :* _)  Nil       = Nothing
    go (a :* as) (b :* bs) =
        pure CompatibleNP
          <*> compatibleClassifier a b
          <*> go as bs

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

compatibleRefl :: (a :~: b) -> Compatible a b
compatibleRefl Refl = CompatibleSame

deferredF1 :: Compatible (f a) (f Deferred)
deferredF1 = CompatibleF1 CompatibleDeferred

deferredF2 :: Compatible (f a b) (f Deferred Deferred)
deferredF2 = CompatibleF2 CompatibleDeferred CompatibleDeferred
