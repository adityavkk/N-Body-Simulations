module Utils
  ( d
  ) where

import DataTypes

-- | Euclidean distance between two positions
d :: Pos -> Pos -> Float
d (P x1 y1) (P x2 y2) = sqrt (dx * dx + dy * dy)
  where
    dx = x2 - x1
    dy = y2 - y1
