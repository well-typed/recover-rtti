{-# OPTIONS_GHC -Wall #-}

module Gen (generateAll) where

import Data.List hiding (uncons)
import Text.Printf

maxTupleSize :: Int
maxTupleSize = 62

vars :: String -> [Int] -> String
vars sep = intercalate sep . map (\i -> "x" ++ show i)

tupleTypeFamily :: String
tupleTypeFamily = intercalate "\n" [
      "type family Tuple xs where"
    , intercalate "\n" [
          intercalate "\n" [
              "  Tuple '[" ++ vars ", " [1 .. n] ++ "] = "
            , "    {- " ++ printf "%02d" n ++ " -} (" ++ vars ", " [1 .. n] ++ ")"
            ]
        | n <- [0 .. maxTupleSize]
        ]
    ]

tooBig :: String
tooBig = intercalate "\n" [
      "data TooBig (n :: Nat) where"
    , concat [
          "  TooBig :: TooBig "
        , foldl (.) id (replicate (maxTupleSize + 1) (\x -> "('S " ++ x ++ ")")) "n"
        ]
    ]

cons :: String
cons = intercalate "\n" [
      "cons :: forall x xs. SListI xs"
    , "  => Proxy xs"
    , "  -> ValidSize (Length (x ': xs))"
    , "  -> (x, Tuple xs) -> Tuple (x ': xs)"
    , "cons _ (ValidSize _n notTooBig) = go shape"
    , "  where"
    , "    go :: Shape xs -> (x, Tuple xs) -> Tuple (x ': xs)"
    , intercalate "\n" [
          concat [
              "    go "
              -- /Input/ tuple size is one smaller
            , foldl (.) id (replicate (i - 1) (\x -> "(ShapeCons " ++ x ++ ")")) "ShapeNil"
            , " (x1, ("
            , vars ", " [2 .. i]
            , ")) =\n        ("
            , vars ", " [1 .. i]
            , ")"
            ]
        | i <- [1 .. maxTupleSize] -- /Result/ tuple size
        ]
    , concat [
          "    go "
        , foldl (.) id (replicate maxTupleSize (\x -> "(ShapeCons " ++ x ++ ")")) "_"
        , " _ =\n"
        , "        notTooBig (TooBig :: TooBig (Length (x ': xs)))"
        ]
    ]

uncons :: String
uncons = intercalate "\n" [
      "uncons :: forall x xs. SListI xs"
    , "  => Proxy xs"
    , "  -> ValidSize (Length (x ': xs))"
    , "  -> Tuple (x ': xs) -> (x, Tuple xs)"
    , "uncons _ (ValidSize _n notTooBig) = go shape"
    , "  where"
    , "    go :: Shape xs -> Tuple (x ': xs) -> (x, Tuple xs)"
    , intercalate "\n" [
          concat [
              "    go "
              -- /Input/ tuple size is one smaller
            , foldl (.) id (replicate (i - 1) (\x -> "(ShapeCons " ++ x ++ ")")) "ShapeNil"
            , " ("
            , vars ", " [1 .. i]
            , ") =\n        "
            , " (x1, ("
            , vars ", " [2 .. i]
            , "))"
            ]
        | i <- [1 .. maxTupleSize] -- /Result/ tuple size
        ]
    , concat [
          "    go "
        , foldl (.) id (replicate maxTupleSize (\x -> "(ShapeCons " ++ x ++ ")")) "_"
        , " _ =\n"
        , "        notTooBig (TooBig :: TooBig (Length (x ': xs)))"
        ]
    ]

liftValidSize :: String
liftValidSize = intercalate "\n" [
      "liftValidSize :: forall n. ValidSize n -> Dict IsValidSize n"
    , "liftValidSize (ValidSize n notTooBig) = go n"
    , "  where"
    , "    go :: Sing n -> Dict IsValidSize n"
    , intercalate "\n" [
          concat [
              "    go "
            , foldl (.) id (replicate i (\x -> "(SS " ++ x ++ ")")) "SZ"
            , " =\n        Dict"
            ]
        | i <- [0 .. maxTupleSize]
        ]
    , concat [
          "    go "
        , foldl (.) id (replicate (maxTupleSize + 1) (\x -> "(SS " ++ x ++ ")")) "_"
        , " =\n"
        , "        notTooBig (TooBig :: TooBig n)"
        ]
    ]

instances :: String
instances = intercalate "\n\n" [
      intercalate "\n" [
          concat [
               "instance IsValidSize "
             , foldl (.) id (replicate i (\n -> "('S " ++ n ++ ")")) "'Z"
             , " where"
             ]
        , "  isValidSize = ValidSize sing $ \\case"
        ]
    | i <- [0 .. maxTupleSize]
    ]

{-
toValidSize :: Int -> Maybe (Some ValidSize)
toValidSize = go
  where
    go :: Int -> Maybe (Some ValidSize)
    go 0 = Just . Some $ isValidSize @'Z
-}

toValidSize :: String
toValidSize = intercalate "\n" [
      "toValidSize :: Int -> Maybe (Some ValidSize)"
    , "toValidSize = go"
    , "  where"
    , "    go :: Int -> Maybe (Some ValidSize)"
    , intercalate "\n" [
          intercalate "\n" [
              "    go " ++ show i ++ " = Just . Some $ "
            , concat [
                  "        "
                , "isValidSize @"
                , foldl (.) id (replicate i (\n -> "('S " ++ n ++ ")")) "'Z"
                ]
           ]
        | i <- [0 .. maxTupleSize]
        ]
    , "    go _ = Nothing"
    ]

generateAll :: IO ()
generateAll = do
    writeFile "size" $ intercalate "\n\n" [
        tooBig
      , liftValidSize
      , instances
      , toValidSize
      ]
    writeFile "recursive" $ intercalate "\n\n" [
        tupleTypeFamily
      , cons
      , uncons
      ]
