module Gravity
  ( moveUniv
  ) where

import DataTypes

type DT = Float

-- | Gravitational constant
gConst :: Float
gConst = 6.674286e-11

-- | Softening parameter to prevent singularities at close range
epsilon :: Float
epsilon = 1.0e-3

-- | Euclidean distance between two positions
dist :: Pos -> Pos -> Float
dist (P x1 y1) (P x2 y2) = sqrt (dx * dx + dy * dy)
  where dx = x2 - x1
        dy = y2 - y1

-- | Gravitational acceleration on a body due to all other bodies
force :: Body -> [Body] -> Acc
force b bs = foldr vSum (A 0 0) [pairForce b b' | b' <- bs, b' /= b]
  where
    vSum :: Acc -> Acc -> Acc
    vSum (A x y) (A x' y') = A (x + x') (y + y')

-- | Pairwise gravitational acceleration from one body on another
pairForce :: Body -> Body -> Acc
pairForce (B _ p1@(P x1 y1) _ _) (B m2 p2@(P x2 y2) _ _)
  | d <= epsilon = A 0 0
  | otherwise    = A gFx gFy
  where d   = dist p1 p2
        gF  = gConst * m2 / (d * d)
        dx  = x2 - x1
        dy  = y2 - y1
        gFx = gF * (dx / d)
        gFy = gF * (dy / d)

-- | Update velocity given acceleration over time step
accel :: Vel -> Acc -> DT -> Vel
accel (V x y) (A accX accY) dt = V (x + dt * accX) (y + dt * accY)

-- | Apply acceleration to a body
accelBody :: Body -> DT -> Acc -> Body
accelBody (B m p v c) dt acc = B m p (accel v acc dt) c

-- | Move a body according to its velocity over a time step
moveBody :: Body -> DT -> Body
moveBody (B m (P x y) v@(V velX velY) c) dt =
  B m (P (x + dt * velX) (y + dt * velY)) v c

-- | Advance the entire universe by one time step
moveUniv :: Float -> Universe -> Universe
moveUniv t u@(U _ _ t' bs) =
  u { bodies = [moveBody (accelBody b dt (force b bs)) dt | b <- bs] }
  where dt = t * t'
