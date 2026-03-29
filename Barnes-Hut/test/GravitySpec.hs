module GravitySpec (spec) where

import Test.Hspec
import DataTypes
import Gravity
import BarnesHut (makeBarnes)
import qualified Graphics.Gloss as G

mkBody :: Float -> Float -> Float -> Body
mkBody m x y = B m (P x y) (V 0 0) G.white Nothing []

spec :: Spec
spec = describe "Gravity" $ do
  describe "force" $ do
    it "returns zero acceleration for an empty tree" $ do
      let b    = mkBody 1e30 0 0
          tree = makeBarnes 100 []
          acc  = force b tree
      ax acc `shouldBe` 0
      ay acc `shouldBe` 0

    it "produces non-zero acceleration between two separated bodies" $ do
      let b1   = mkBody 1e30 0 0
          b2   = mkBody 1e30 100 0
          tree = makeBarnes 1000 [b1, b2]
          acc  = force b1 tree
      -- b2 is at positive x, so acceleration should be positive in x
      ax acc `shouldSatisfy` (> 0)

    it "produces symmetric forces for equal masses" $ do
      let b1   = mkBody 1e30 (-50) 0
          b2   = mkBody 1e30 50 0
          tree = makeBarnes 1000 [b1, b2]
          acc1 = force b1 tree
          acc2 = force b2 tree
      -- Equal masses: forces should be equal and opposite
      abs (ax acc1 + ax acc2) `shouldSatisfy` (< 1e-10)

  describe "pairForce" $ do
    it "obeys inverse-square law (doubling distance -> 1/4 force)" $ do
      let b0   = mkBody 1e30 0 0
          bNear = mkBody 1e30 100 0
          bFar  = mkBody 1e30 200 0
          A fNear _ = pairForce b0 bNear
          A fFar  _ = pairForce b0 bFar
          ratio = fNear / fFar
      -- Should be approximately 4
      abs (ratio - 4.0) `shouldSatisfy` (< 0.01)
