module Gravity
  ( moveUniv
  , force
  , pairForce
  , theta
  ) where

import DataTypes
import Utils
import BarnesHut (makeBarnes)
import qualified Graphics.Gloss as G

type DT = Float

-- | Barnes-Hut accuracy parameter.
--   Smaller = more accurate but slower. 0.5 is typical; 0.2 is high accuracy.
theta :: Float
theta = 0.5

-- | Gravitational constant
gConst :: Float
gConst = 6.674286e-11

-- | Softening parameter to prevent singularities
epsilon :: Float
epsilon = 1.0e-3

-- | Calculate acceleration on a body using the Barnes-Hut tree
force :: Body -> BarnesTree -> Acc
force _ (Exter (Leaf _ _)) = A 0 0
force b (Exter n)
  | b' /= b   = pairForce b b'
  | otherwise  = A 0 0
  where b' = body n
force b (Inter cmass _c w _m q1 q2 q3 q4)
  | wd < theta = pairForce b (B (btMass tree) cmass (V 0 0) G.red (Just 0) [])
  | otherwise   = foldr (vSum . force b) (A 0 0) [q1, q2, q3, q4]
  where
    tree = Inter cmass _c w _m q1 q2 q3 q4
    wd   = w / dist
    dist = d (pos b) cmass
    vSum :: Acc -> Acc -> Acc
    vSum (A x y) (A x' y') = A (x + x') (y + y')

-- | Pairwise gravitational acceleration
pairForce :: Body -> Body -> Acc
pairForce (B _ p1@(P x1 y1) _ _ _ _) (B m2 p2@(P x2 y2) _ _ _ _)
  | dist' <= epsilon = A 0 0
  | otherwise        = A gFx gFy
  where dist' = d p1 p2
        gF    = gConst * m2 / (dist' * dist')
        dx    = x2 - x1
        dy    = y2 - y1
        gFx   = gF * (dx / dist')
        gFy   = gF * (dy / dist')

-- | Update velocity given acceleration
accel :: Vel -> Acc -> DT -> Vel
accel (V x y) (A accX accY) dt = V (x + dt * accX) (y + dt * accY)

-- | Apply acceleration to a body over time step
accelBody :: Body -> DT -> Acc -> Body
accelBody (B m p v c s t) dt acc = B m p (accel v acc dt) c s t

-- | Move a body according to its velocity, optionally recording trail
moveBody :: Bool -> Body -> DT -> Body
moveBody addTrail (B m (P x y) v@(V velX velY) c s t) dt
  | addTrail  = B m (P newX newY) v c s ((x, y) : take 200 t)
  | otherwise = B m (P newX newY) v c s []
  where newX = x + dt * velX
        newY = y + dt * velY

-- | Advance the universe by one time step, rebuilding the Barnes-Hut tree
moveUniv :: Float -> Universe -> Universe
moveUniv t u@(U _ _ t' bs bt tls) =
  u { bodies     = bs'
    , barnesTree = bt'
    }
  where dt  = t * t'
        bs' = [moveBody tls (accelBody b dt (force b bt)) dt | b <- bs]
        bt' = makeBarnes 7e12 bs'
