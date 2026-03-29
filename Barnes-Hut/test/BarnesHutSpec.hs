module BarnesHutSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import DataTypes
import BarnesHut
import qualified Graphics.Gloss as G

-- | Helper to create a simple body at a given position
mkBody :: Float -> Float -> Float -> Body
mkBody m x y = B m (P x y) (V 0 0) G.white Nothing []

spec :: Spec
spec = describe "BarnesHut" $ do
  describe "makeBarnes" $ do
    it "creates a tree from an empty list that is an internal node" $ do
      let tree = makeBarnes 100 []
      case tree of
        Inter {} -> return () :: IO ()
        _        -> expectationFailure "Expected internal node for empty tree"

    it "inserts a single body into the tree" $ do
      let b    = mkBody 1e30 10 20
          tree = makeBarnes 100 [b]
      -- The tree should contain our body somewhere
      bodyInTree b tree `shouldBe` True

    it "inserts multiple bodies without crashing" $ do
      let bodies = [ mkBody 1e30 10 20
                   , mkBody 1e28 (-30) 40
                   , mkBody 1e25 5 (-15)
                   ]
          tree = makeBarnes 100 bodies
      -- All bodies should be findable
      all (\b -> bodyInTree b tree) bodies `shouldBe` True

  describe "inQuad" $ do
    it "correctly identifies a body inside a quadrant" $ do
      let b    = mkBody 1 5 5
          leaf = bLeaf (P 5 5) 20
      inQuad b leaf `shouldBe` True

    it "correctly identifies a body outside a quadrant" $ do
      let b    = mkBody 1 100 100
          leaf = bLeaf (P 0 0) 10
      inQuad b leaf `shouldBe` False

  describe "properties" $ do
    it "total mass is conserved after insertion" $ property $
      \(Positive n) -> n < 50 ==>
        let bodies = [mkBody (fromIntegral (i :: Int)) (fromIntegral i) (fromIntegral i) | i <- [1..n]]
            tree   = makeBarnes 10000 bodies
            totalInputMass = sum (map mass bodies)
        in abs (treeMass tree - totalInputMass) < 1e-3

-- | Check if a body exists somewhere in the tree (by position match)
bodyInTree :: Body -> BarnesTree -> Bool
bodyInTree b (Exter (Leaf _ _)) = False
bodyInTree b (Exter (Node _ _ _ _ b')) = pos b == pos b'
bodyInTree b (Inter _ _ _ _ q1 q2 q3 q4) =
  any (bodyInTree b) [q1, q2, q3, q4]

-- | Get total mass stored in a tree
treeMass :: BarnesTree -> Float
treeMass (Exter (Leaf _ _)) = 0
treeMass (Exter (Node _ _ _ m _)) = m
treeMass (Inter _ _ _ m _ _ _ _) = m
