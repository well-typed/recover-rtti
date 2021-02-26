{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Debug.RecoverRTTI.Show (
    canShowClassified
  , showAnything
  ) where

import Data.SOP
import Data.SOP.Dict

import Debug.RecoverRTTI.Classify
import Debug.RecoverRTTI.ConstrInfo

canShowClassified :: Classifier a -> Dict Show a
canShowClassified = \case
    C_Void   -> Dict
    C_Unit   -> Dict
    C_Bool   -> Dict
    C_Int    -> Dict
    C_Char   -> Dict
    C_Double -> Dict
    C_List c -> case canShowClassified c of Dict -> Dict
    C_Custom ProxyKnownConstr -> Dict

instance Show Classified where
  showsPrec p (Classified c x) =
     case canShowClassified c of
       Dict -> showsPrec p x

instance KnownConstr c => Show (UserDefined c) where
  showsPrec p =
        showParen (p >= 11)
      . (showString constrName .)
      . foldl (.) id
      . map (\x -> showString " " . showsPrec 11 x)
      . fromUserDefined
    where
      ConstrInfo{constrName} = knownConstr (Proxy @c)

showAnything :: a -> String
showAnything = show . classified
