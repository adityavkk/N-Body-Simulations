module Utils where

import DataTypes

d :: Pos -> Pos -> Float
d (P x1 y1) (P x2 y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2 - x1
    dy = y2 - y1

distance :: Body -> Body -> Float
distance b1 b2 = d (pos b1) (pos b2)

scalePos :: Pos -> Float -> Float -> Pos
scalePos (P x y) sx sy = P (sx * x) (sy * y)

scaleVel :: Vel -> Float -> Float -> Vel
scaleVel (V x y) sx sy = V (sx * x) (sy * y)

totMass :: [Body] -> Float
totMass = sum . map mass
