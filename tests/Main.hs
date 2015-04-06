module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Hspec

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
