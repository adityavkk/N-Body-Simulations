module BarnesHut where

import GHC.Float
import DataTypes

cm :: Body -> (Mass, Pos) -> Pos
cm (B m1 (P x1 y1) _ _ t _) (m2, P x2 y2) = P x y
  where [m1', x1', y1', m2', x2', y2'] = map float2Double [m1, x1, y1, m2, x2, y2]
        m = m1' + m2'
        x = double2Float $ (x1' * m1' + x2' * m2') / m
        y = double2Float $ (y1' * m1' + y2' * m2') / m

makeBarnes :: Float -> [Body] -> BarnesTree
makeBarnes w = foldr insert $ mtBarnes w

mtBarnes :: Float -> BarnesTree
mtBarnes w = interNode (P 0 0) (P 0 0) w 0

interNode cm c@(P x y) w m = Inter cm c w m (bLeaf (P (x - w'') (y + w'')) w')
                                            (bLeaf (P (x + w'') (y + w'')) w')
                                            (bLeaf (P (x - w'') (y - w'')) w')
                                            (bLeaf (P (x + w'') (y - w'')) w')
  where w'  = w / 2
        w'' = w' / 2

bLeaf :: Pos -> Float -> BarnesTree
bLeaf = (Exter .) . Leaf

insert :: Body -> BarnesTree -> BarnesTree
insert b (Exter (Leaf c w))              = Exter (Node (pos b) c w (mass b) b)
insert b1 (Exter (Node cMass c w m b2))  =
  insert b1 $ insert b2 $ interNode cMass c w m
insert b (Inter cMass c w m nw ne sw se) = Inter cMass' c w m' nw' ne' sw' se'
  where
    cMass' = cm b (m, cMass)

    m'     = m + mass b

    [nw', ne', sw', se'] = foldr f [] [nw, ne, sw, se]

    f :: BarnesTree -> [BarnesTree] -> [BarnesTree]
    f bt bts
      | inQuad b bt = insert b bt : bts
      | otherwise   = bt : bts

inQuad :: Body -> BarnesTree -> Bool
inQuad (B _ (P x y) _ _ _ _) bt = case bt of
                                (Exter (Leaf (P x' y') w))       ->
                                  inRange x y x' y' w
                                (Exter (Node _ (P x' y') w _ _)) ->
                                  inRange x y x' y' w
                                (Inter _ (P x' y') w _ _ _ _ _)  ->
                                  inRange x y x' y' w

inRange :: (Ord t, Fractional t) => t -> t -> t -> t -> t -> Bool
inRange x y x' y' w = x > xmn && x < xmx && y >= ymn && y <= ymx
  where xmn = x' - (w / 2)
        xmx = x' + (w / 2)
        ymn = y' - (w / 2)
        ymx = y' + (w / 2)
