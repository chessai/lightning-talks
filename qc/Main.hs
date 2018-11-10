{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import Data.Foldable
import Data.Functor.Classes
import Data.Monoid
import Data.Proxy (Proxy(Proxy))
import Data.Map.Strict (Map)

import Test.QuickCheck
import Test.QuickCheck.Classes (Laws(Laws))
import qualified Test.QuickCheck.Classes as QCC
import Test.Tasty.QuickCheck as TQC
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  
  QCC.lawsCheckMany
    [ ("Int", mkLaws pInt [QCC.eqLaws, QCC.ordLaws])
    , ("Sum Int", mkLaws pSumInt [QCC.monoidLaws])
    , ("Map Int", mkLaws pMapInt [QCC.functorLaws])
    , ("Map Int Int", mkLaws pMapIntInt [QCC.monoidLaws, QCC.isListLaws])
    --, ("UnlawfulList", mkLaws pUnlawfulList [QCC.foldableLaws])
    ]

{-
  defaultMain $ testGroup "properties"
    [ lawsToTest "Int" (mkLaws pInt [QCC.eqLaws, QCC.ordLaws])
    , lawsToTest "Sum Int" (mkLaws pSumInt [QCC.monoidLaws])
    , lawsToTest "Map Int" (mkLaws pMapInt [QCC.functorLaws])
    , lawsToTest "Map Int Int" (mkLaws pMapIntInt [QCC.monoidLaws, QCC.isListLaws])
    --, lawsToTest "UnlawfulList" (mkLaws pUnlawfulList [QCC.foldableLaws])
    ]
-}

-----------------------------------------------------------------------------

newtype UnlawfulList a = UnlawfulList [a]
  deriving (Eq, Ord, Eq1, Ord1, Show, Show1, Arbitrary, Arbitrary1)

instance Foldable UnlawfulList where
  foldl' f b (UnlawfulList x) = foldl f b x
  foldMap f (UnlawfulList x) = foldMap f x

pUnlawfulList = Proxy @UnlawfulList

-----------------------------------------------------------------------------

pMapIntInt = Proxy @(Map Int Int)
pMapInt = Proxy @(Map Int)
pInt = Proxy @Int
pSumInt = Proxy @(Sum Int)

mkLaws :: Proxy (a :: k) -> [Proxy a -> Laws] -> [Laws]
mkLaws p fs = map ($ p) fs

pairsToTestTree :: [(String, Property)] -> [TestTree]
pairsToTestTree pairs = map (uncurry TQC.testProperty) pairs

lawsToTest :: TestName -> [Laws] -> TestTree
lawsToTest testName laws = testGroup testName (map lawToTest laws)

lawToTest :: Laws -> TestTree
lawToTest (Laws name pairs) = testGroup name (pairsToTestTree pairs)
