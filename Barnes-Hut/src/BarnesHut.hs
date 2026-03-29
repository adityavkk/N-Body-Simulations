module BarnesHut
  ( makeBarnes
  , mtBarnes
  , insert
  , interNode
  , bLeaf
  , inQuad
  ) where

import GHC.Float
import DataTypes

-- | Calculate center of mass when adding a body to an existing mass/position.
--   Uses Double internally for precision.
cm :: Body -> (Mass, Pos) -> Pos
cm (B m1 (P x1 y1) _ _ _ _) (m2, P x2 y2) = P x y
  where m1' = float2Double m1
        x1' = float2Double x1
        y1' = float2Double y1
        m2' = float2Double m2
        x2' = float2Double x2
        y2' = float2Double y2
        m   = m1' + m2'
        x   = double2Float $ (x1' * m1' + x2' * m2') / m
        y   = double2Float $ (y1' * m1' + y2' * m2') / m

-- | Build a Barnes-Hut quadtree from a list of bodies
makeBarnes :: Float -> [Body] -> BarnesTree
makeBarnes w = foldr insert $ mtBarnes w

-- | Empty Barnes-Hut tree with given width
mtBarnes :: Float -> BarnesTree
mtBarnes w = interNode (P 0 0) (P 0 0) w 0

-- | Create an internal node with four empty quadrant leaves
interNode :: Pos -> Pos -> Float -> Mass -> BarnesTree
interNode cmass c@(P x y) w m =
  Inter cmass c w m
    (bLeaf (P (x - w'') (y + w'')) w')
    (bLeaf (P (x + w'') (y + w'')) w')
    (bLeaf (P (x - w'') (y - w'')) w')
    (bLeaf (P (x + w'') (y - w'')) w')
  where w'  = w / 2
        w'' = w' / 2

-- | Create an external leaf node
bLeaf :: Pos -> Float -> BarnesTree
bLeaf = (Exter .) . Leaf

-- | Insert a body into the Barnes-Hut tree
insert :: Body -> BarnesTree -> BarnesTree
insert b (Exter (Leaf c w)) =
  Exter (Node (pos b) c w (mass b) b)
insert b1 (Exter (Node cmass c w m b2)) =
  insert b1 $ insert b2 $ interNode cmass c w m
insert b (Inter cmass c w m nw' ne' sw' se') =
  Inter cmass' c w m' nwNew neNew swNew seNew
  where
    cmass' = cm b (m, cmass)
    m'     = m + mass b
    [nwNew, neNew, swNew, seNew] = foldr f [] [nw', ne', sw', se']

    f :: BarnesTree -> [BarnesTree] -> [BarnesTree]
    f bt bts
      | inQuad b bt = insert b bt : bts
      | otherwise   = bt : bts

-- | Check if a body falls within a tree node's quadrant
inQuad :: Body -> BarnesTree -> Bool
inQuad (B _ (P x y) _ _ _ _) bt = case bt of
  (Exter (Leaf (P x' y') w))       -> inRange x y x' y' w
  (Exter (Node _ (P x' y') w _ _)) -> inRange x y x' y' w
  (Inter _ (P x' y') w _ _ _ _ _)  -> inRange x y x' y' w

-- | Check if point (x,y) is within the square centered at (x',y') with width w
inRange :: (Ord t, Fractional t) => t -> t -> t -> t -> t -> Bool
inRange x y x' y' w = x > xmn && x < xmx && y >= ymn && y <= ymx
  where xmn = x' - (w / 2)
        xmx = x' + (w / 2)
        ymn = y' - (w / 2)
        ymx = y' + (w / 2)
