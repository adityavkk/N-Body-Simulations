module BarnesHut where

import DataTypes

cm :: Body -> (Mass, Pos) -> Pos
cm (B m1 (P x1 y1) _ _) (m2, P x2 y2) = P x y
  where m = m1 + m2
        x = (x1 * m1 + x2 * m2) / m
        y = (y1 * m1 + y2 * m2) / m

makeBarnes :: Float -> [Body] -> BarnesTree
makeBarnes w = foldr insert $ mtBarnes w

mtBarnes :: Float -> BarnesTree
mtBarnes w = Inter (P 0 0) (P 0 0) 0 w (bLeaf (P (-w'') w') w')
                                       (bLeaf (P w'' w'') w')
                                       (bLeaf (P (-w'') (-w'')) w')
                                       (bLeaf (P w'' (-w'')) w')
  where w'  = w / 2
        w'' = w' / 2

bLeaf :: Pos -> Float -> BarnesTree
bLeaf = (Exter .) . Leaf

insert :: Body -> BarnesTree -> BarnesTree
insert b (Exter (Leaf c w))              = Exter (Node (pos b) c w (mass b) b)
insert b1 (Exter (Node cMass c w m b2))  = insert b2 $ insert b1 $ mtBarnes w
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
    inQuad _ (Exter _) = True
    inQuad (B _ (P x y) _ _) bt
      | x < xmn && x > xmx &&
        y < ymn && y > ymx   = True
      | otherwise            = False
      where xmn = x' - (w / 2)
            xmx = x' + (w / 2)
            ymn = y' - (w / 2)
            ymx = y' + (w / 2)
            x'  = px $ btCenter bt
            y'  = py $ btCenter bt


