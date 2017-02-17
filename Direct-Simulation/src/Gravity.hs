module Gravity where

import DataTypes

type DT = Float
type Time = Float

force :: Body -> [Body] -> Acc
force b bs = foldr vSum (A 0 0) [f b b' | b' <- bs, b' /= b]
  where
    vSum (A x y) (A x' y') = A (x + x') (y + y')
    gConst = 6.674286e-11
    e      = 1.0e-3
    f (B m1 p1@(P x1 y1) v1 _) (B m2 p2@(P x2 y2) v2 _)
      | dist <= e = A 0 0
      | otherwise = A gFx gFy
      where dist = d p1 p2
            gF   = gConst * m2 / dist^2
            gFx  = gF * (dx / dist)
            gFy  = gF * (dy / dist)
            d (P x1 y1) (P x2 y2) = sqrt (dx^2 + dy^2)
            dx = x2 - x1
            dy = y2 - y1

accel :: Vel -> Acc -> DT -> Vel
accel (V x y) (A ax ay) dt = V (x + dt * ax) (y + dt * ay)

accelBody :: Body -> DT -> Acc -> Body
accelBody (B m p v c) dt acc = B m p (accel v acc dt) c

moveBody :: Body -> DT -> Body
moveBody (B m (P x y) v@(V vx vy) c) dt = B m (P (x + dt * vx) (y + dt * vy)) v c

moveUniv :: Float -> Universe -> Universe
moveUniv t u@(U _ _ t' bs) = let dt = t * t' in
  u { bodies = [moveBody (accelBody b dt (force b bs)) dt | b <- bs] }
