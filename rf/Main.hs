{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Typeable (typeOf)
import Control.Monad (unless)
import Refined
import qualified Data.Text.Prettyprint.Doc as PP

main :: IO ()
main = do
  pure ()

x :: Int
x = unrefine ($$(refineTH 1) :: Refined Positive Int)

y :: [Int]
y = unrefine ($$(refineTH [1,2,3,4,5]) :: Refined Ascending [Int])

data Overflow = Overflow

instance (Bounded x, Ord x, Show x) => Predicate Overflow x where
  validate p x = do
    unless ((x <= maxBound) && (x >= minBound)) $ do
      let msg = mconcat
            [ "Value is overflown: "
            , "\n  "
            , "value:  "
            , PP.pretty (show (x :: x))
            , "\n  "
            , "minBound:  "
            , PP.pretty (show (minBound :: x))
            , "\n  "
            , ", maxBound:  "
            , PP.pretty (show (maxBound :: x))
            ]
      throwRefineOtherException (typeOf p) msg

