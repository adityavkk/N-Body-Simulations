{-# LANGUAGE FlexibleContexts #-}

module Bodies
  ( solarSystem
  , binaryStars
  , threeBodyCircle
  , fourBodyStar
  , figureEight
  , crazyU
  ) where

import DataTypes
import BarnesHut (makeBarnes)
import qualified Graphics.Gloss as G
import Test.QuickCheck

-- | Reference values
earthM :: Float
earthM = 5.9736e24

earthD :: Float
earthD = 152098232.0e3

earthV :: Float
earthV = 29.78e3

sun :: Body
sun = B 1.9891e30 (P (-1) (-1)) (V 0 0) G.yellow Nothing []

sun' :: Body
sun' = B 3.9891e33 (P (-1) (-1)) (V 0 0) (G.dark G.yellow) (Just 2) []

-- | Random body generators
massGen :: Gen Float
massGen = choose (0.002, 350)

distGen :: Gen Float
distGen = choose (-100, 100)

genDistVel :: Gen (Float, Float)
genDistVel = distGen >>= \n -> return (n, (1 / sqrt (abs n)) * 3)

rP :: Gen (Float, Float, Float, Float, Float, Float)
rP = do
  (dx, vx') <- genDistVel
  (dy, vy') <- genDistVel
  flipx <- oneof [return 1, return (-1)]
  flipy <- oneof [return 1, return (-1)]
  m <- massGen
  _ <- distGen
  return (m, dx, dy, vx' * flipx, vy' * flipy, 0)

rPs :: Int -> Gen [(Float, Float, Float, Float, Float, Float)]
rPs = flip vectorOf rP

bs :: Int -> Gen [Body]
bs n = rPs n >>= \ps ->
  return $ map (\(m, dx, dy, vx', vy', _) ->
    B (earthM * m)
      (P (earthD * dx) (earthD * dy))
      (V (earthV * vx') (earthV * vy'))
      G.azure
      (Just 1)
      []) ps

-- | Generate a random universe with n bodies
crazyU :: Int -> Gen Universe
crazyU n = bs n >>= \ps ->
  return $ U ((125 * 0.1) / earthD)
              1
              2000
              (sun' : ps)
              (makeBarnes 20e12 (sun' : ps))
              False

-- | Planet data
masses :: [Float]
masses = map (* earthM)
  [0.0553, 0.815, 0.0123, 1.0, 0.107, 317.8, 95.2, 14.5, 17.1, 0.0025, 3.6832412e-11]

distances :: [Float]
distances = map (* earthD)
  [0.387, 0.723, 1.00257, 1.0, 1.52, 5.20, 9.58, 19.20, 30.05, 39.48, 35]

initVelocities :: [Float]
initVelocities = map (* earthV)
  [1.59, 1.18, 1.0343, 1.0, 0.808, 0.439, 0.325, 0.228, 0.182, 0.157, 2.9407e-2]

colors :: [G.Color]
colors = [ G.greyN 0.5, G.violet, G.greyN 0.2, G.blue, G.red
         , G.orange, G.chartreuse, G.azure, G.cyan, G.magenta, G.greyN 0.2 ]

fx :: Float -> Float -> Float -> G.Color -> Body
fx m d' v c = B m (P 1 d') (V v 0) c (Just m) []

fy :: Float -> Float -> Float -> G.Color -> Body
fy m d' v c = B m (P d' 1) (V 0 v) c (Just 4.5) []

planets :: [Body]
planets = zipWith4 fy masses distances initVelocities colors

-- ── Preset Scenarios ─────────────────────────────────────────────

solarSystem :: Universe
solarSystem = U ((125 * 0.4) / earthD) 1 2000
                (sun : planets)
                (makeBarnes w (sun : planets))
                False
  where w = 20e12

fourBodyStar :: Universe
fourBodyStar = U (500 / earthD) 1.66e-28 1500 bods (makeBarnes w bods) False
  where ms  = [3.0e28, 3.0e28, 3.0e28, 3.0e28]
        ds  = [-3.5e10, -1.0e10, 1.0e10, 3.5e10]
        vs  = [1.4e03, 1.4e04, (-1.4e04), (-1.4e03)]
        cs  = [G.violet, G.chartreuse, G.red, G.orange]
        bods = zipWith4 fy ms ds vs cs
        w   = 5.0e10

threeBodyCircle :: Universe
threeBodyCircle = U (500 / earthD) 1.17e-24 1000 bods' (makeBarnes w bods') False
  where ms  = [5.97e24, 1.989e30, 1.989e30]
        ds  = [0.0e00, 4.5e10, (-4.5e10)]
        vs  = [0.05e04, 3.0e04, (-3.0e04)]
        cs  = [G.blue, G.yellow, G.yellow]
        bods = zipWith4 fx ms ds vs cs
        bods' = [ (bods !! 1) { size = Nothing }
                , (bods !! 2) { size = Nothing }
                , bods !! 0
                ]
        w   = 1.25e11

binaryStars :: Universe
binaryStars = U (500 / earthD) 4.33e-30 2000 bods (makeBarnes w bods) False
  where ms  = [1.5e30, 1.5e30]
        ds  = [4.5e10, (-4.5e10)]
        vs  = [1.0e04, (-1.0e04)]
        cs  = [G.red, G.green]
        bods = zipWith4 fx ms ds vs cs
        w   = 5.0e10

figureEight :: Universe
figureEight = U (500 / earthD) 1.33e-28 500 bods (makeBarnes w bods) False
  where ms    = [1.989e30, 1.989e30, 1.989e30]
        poss  = [(9.7e10, -2.43e10), (-9.7e10, 2.43e10), (0, 0)]
        vels  = [(3.66e04, 2.32e04), (3.66e04, 2.32e04), (-6.32e04, -5.64e04)]
        cs    = [G.blue, G.red, G.yellow]
        bods  = zipWith4 zipF ms poss vels cs
        w     = 1.25e11
        zipF m (x, y) (vx', vy') c = B m (P x y) (V vx' vy') c (Just m) []
