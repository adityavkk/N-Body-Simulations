module Main (main) where

import Test.Hspec

import qualified BarnesHutSpec
import qualified GravitySpec

main :: IO ()
main = hspec $ do
  BarnesHutSpec.spec
  GravitySpec.spec
