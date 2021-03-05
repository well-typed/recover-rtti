{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Debug.RecoverRTTI.Show (showAnything) where

import Data.SOP
import Data.SOP.Dict
import System.IO.Unsafe (unsafePerformIO)

import Debug.RecoverRTTI.Classify
import Debug.RecoverRTTI.Constr
import Debug.RecoverRTTI.FlatClosure
import Debug.RecoverRTTI.Util
import Debug.RecoverRTTI.Util.TypeLevel

{-------------------------------------------------------------------------------
  User-facing API
-------------------------------------------------------------------------------}

-- | Show any value
showAnything :: forall a. a -> String
showAnything x = showClassifiedValue 0 (classified x) ""

{-------------------------------------------------------------------------------
  Classifiers and classified values
-------------------------------------------------------------------------------}

deriving instance Show (Classifier a)
deriving instance Show (MaybeEmpty Classified a)
deriving instance Show (Some Classified)

instance SListI xs => Show (Classifiers xs) where
  show (Classifiers xs) = go (hpure Dict)
    where
      go :: NP (Dict (Compose Show Classifier)) xs -> String
      go dicts =
          case all_NP dicts of
            Dict -> "(" ++ show xs ++ ")"

instance Show (Classified a) where
  showsPrec p (Classified c x) = showParen (p >= 11) $
      case canShowClassified c of
        Dict ->
            showString "Classified "
          . showsPrec 11 c
          . showsPrec 11 x

{-------------------------------------------------------------------------------
  User-defined
-------------------------------------------------------------------------------}

instance KnownConstr c => Show (UserDefined c) where
  showsPrec p x =
      case fromUserDefined x of
        [] -> showString constrName
        xs -> showParen (p >= 11)
            . (showString constrName .)
            . foldl (.) id
            . map (\(Exists x') -> showString " " . showClassifiedValue 11 x')
            $ xs
    where
      Constr{constrName} = knownConstr (sing @_ @c)

{-------------------------------------------------------------------------------
  Unknown values
-------------------------------------------------------------------------------}

-- | If classification failed, we show the closure itself
instance Show Unknown where
  showsPrec p (Unknown x) = unsafePerformIO $ do
      closure <- getBoxedClosureData (asBox x)
      return $ showsPrec p closure

{-------------------------------------------------------------------------------
  Reference cells

  Unfortunately reference cells are moved by GC, so we can't do much here;
  showing the address of the variable isn't particularly helpful.
-------------------------------------------------------------------------------}

instance Show SomeSTRef where
  show _ = "<STRef/IORef>" -- they look the same on the heap

instance Show SomeMVar where
  show _ = "<MVar>"

instance Show SomeTVar where
  show _ = "<TVar>"

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

instance Show SomeFun where
  show _ = "<Fun>"

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Show the classified value (without the classifier)
showClassifiedValue :: Int -> Classified a -> ShowS
showClassifiedValue p (Classified c x) =
    case canShowClassified c of
      Dict -> showsPrec p x

canShowClassified :: Classifier a -> Dict Show a
canShowClassified = \case
    -- Primitive types

    C_Bool     -> Dict
    C_Char     -> Dict
    C_Double   -> Dict
    C_Float    -> Dict
    C_Int      -> Dict
    C_Int16    -> Dict
    C_Int8     -> Dict
    C_Int32    -> Dict
    C_Int64    -> Dict
    C_Ordering -> Dict
    C_Unit     -> Dict
    C_Word     -> Dict
    C_Word8    -> Dict
    C_Word16   -> Dict
    C_Word32   -> Dict
    C_Word64   -> Dict

    -- String types

    C_String      -> Dict
    C_BS_Strict   -> Dict
    C_BS_Lazy     -> Dict
    C_BS_Short    -> Dict
    C_Text_Strict -> Dict
    C_Text_Lazy   -> Dict

    -- Compound

    C_List Empty -> Dict
    C_List (NonEmpty (Classified c _)) -> case canShowClassified c of Dict -> Dict

    -- Reference cells

    C_STRef -> Dict
    C_TVar  -> Dict
    C_MVar  -> Dict

    -- Functions

    C_Fun -> Dict

    -- User-defined

    C_Custom SConstr -> Dict

    -- Classification failed

    C_Unknown -> Dict
