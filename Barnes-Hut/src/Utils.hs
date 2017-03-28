module Utils where

import DataTypes

d :: Pos -> Pos -> Float
d (P x1 y1) (P x2 y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2 - x1
    dy = y2 - y1
