module Gravity where

import DataTypes
import Utils
import Constants
import GHC.Float
import BarnesHut hiding (insert)
import qualified Graphics.Gloss as G

type DT   = Float
type Time = Float

theta = 0.1

sd :: BarnesTree -> Body -> Float
sd = undefined

force :: Body -> BarnesTree -> Acc
force b (Exter (Leaf _ _)) = A 0 0
force b (Exter n)
  | b' /= b   = f b b'
  | otherwise = A 0 0
  where
    b' = body n
force b (Inter cMass c w m q1 q2 q3 q4)
  | wd < theta = f b (B m cMass (V 0 0) G.red [])
  | otherwise  = foldr (vSum . force b) (A 0 0) [q1, q2, q3, q4]
  where
    wd                     = w / dist
    dist                   = d (pos b) cMass
    vSum (A x y) (A x' y') = A (x + x') (y + y')

f :: Body -> Body -> Acc
f (B m1 p1@(P x1 y1) _ _ _) (B m2 p2@(P x2 y2) _ _ _)
  | dist <= e = A 0 0
  | otherwise = A gFx gFy
  where dist   = d p1 p2
        gConst = 1.0
        e      = 1.0e-5
        gF     = gConst * m2 / dist^2
        gFx    = gF * (dx / dist)
        gFy    = gF * (dy / dist)
        dx     = x2 - x1
        dy     = y2 - y1

accel :: Vel -> Acc -> DT -> Vel
accel (V x y) (A ax ay) dt = V (x + dt * ax) (y + dt * ay)

accelBody :: Body -> DT -> Acc -> Body
accelBody (B m p v c t) dt acc = B m p (accel v acc dt) c t

moveBody :: Bool -> Body -> DT -> Body
moveBody tail (B m (P x y) v@(V vx vy) c t) dt
  | tail = B m (P (x + dt * vx) (y + dt * vy)) v c ((x, y):t)
  | otherwise = B m (P (x + dt * vx) (y + dt * vy)) v c []

moveUniv :: Float -> Universe -> Universe
moveUniv t u@(U _ _ t' m bs bt w tls) = u { bodies     = bs'
                                          , barnesTree = bt'}
    where dt   = double2Float $ float2Double t / (sqrt (w'^3 / (mTot * gConst))) * 2.0e5
          bs'  = [moveBody tls (accelBody b dt (force b bt)) dt | b <- bs]
          bt'  = makeBarnes 7e12 bs'
          w'   = float2Double w
          mTot = float2Double m
