{-# LANGUAGE FlexibleContexts #-}
module Bodies where

import DataTypes
import Gravity
import BarnesHut hiding (insert)
import Data.List hiding (insert)
import qualified Graphics.Gloss as G
import Test.QuickCheck

massGen = choose (0.002, 350)
distGen = choose (-100, 100)

genDistVel :: Gen (Float, Float)
genDistVel = distGen >>= f
  where f n = return (n, (1 / (sqrt $ abs n)) * 3 :: Float)

rP :: Gen (Float, Float, Float, Float, Float, Float)
rP = do
  (dx, vx) <- genDistVel
  (dy, vy) <- genDistVel
  flipx <- oneof [return 1, return (-1)]
  flipy <- oneof [return 1, return (-1)]
  mass <- massGen
  d' <- distGen
  return (mass, dx, dy, vx * flipx, vy * flipy, d')

rPs :: Int -> Gen [(Float, Float, Float, Float, Float, Float)]
rPs n = vectorOf n rP

bs n = rPs n >>=
  (\ ps -> return $ map (\ (m, dx, dy, vx, vy, y) -> B (earthM * m)
                                             (P (earthD * dx) (earthD * dy))
                                             (V (earthV * vx) (earthV * vy))
                                             G.azure
                                             []) ps)

crazyU :: Int -> Gen Universe
crazyU n = bs n >>=
  (\ ps ->
    return $ U ((125 * 0.1) / 152098232.0e3)
                13.97e27
                2000
                (sun':ps)
                (makeBarnes 20e12 (sun':ps))
                False)
  where jupiter = planets !! 5

sun        = B 1.9891e30 (P (-1) (-1)) (V 0 0) G.yellow []
sun'       = B 3.9891e33 (P (-1) (-1)) (V 0 0) G.yellow []
earthM     = 5.9736e24
earthD     = 152098232.0e3
earthV     = 29.78e3
masses =
  map (* earthM)
    [0.0553, 0.815, 0.0123, 1.0, 0.107, 317.8, 95.2, 14.5, 17.1, 0.0025]
distances =
  map (* earthD)
    [0.387, 0.723, 1.00257, 1.0, 1.52, 2.20, 9.58, 19.20, 30.05, 39.48]
initVelocities =
  map (* earthV)
    [1.59, 1.18, 1.0343, 1.0, 0.808, 0.439, 0.325, 0.228, 0.182, 0.157]
colors = [G.greyN 0.5, G.violet, G.greyN 0.2, G.blue, G.red, G.orange, G.chartreuse, G.azure, G.cyan, G.magenta]

planets = zipWith4 fy masses distances initVelocities colors

fy m d v c = B m (P d 1) (V 0 v) c []
fx m d v c = B m (P 1 d) (V v 0) c []

solarSystem = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:planets)
                (makeBarnes w (sun:planets))
                False
  where w = 20e12

doublePs n ps = foldr
  (\ p@(B m (P x y) (V vx vy) _ _) ps -> p {pos = P (x / n) y } : p : ps)
                 [] ps

lotaPlanets n = last $ take n $ iterate (doublePs 2) planets

lotaSystem = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:lop)
                (makeBarnes w (sun:lop))
  where w = 20e12
        lop = lotaPlanets 10

fourBodyStar = U (500 / earthD) 13.97e27 1500 bs (makeBarnes w bs) False
  where masses         = [3.0e28, 3.0e28, 3.0e28, 3.0e28]
        distances      = [-3.5e10, -1.0e10, 1.0e10, 3.5e10]
        initVelocities = [1.4e03, 1.4e04, (-1.4e04), (-1.4e03)]
        colors         = [G.violet, G.chartreuse, G.red, G.orange]
        bs             = zipWith4 fy masses distances initVelocities colors
        w              = 5.0e10

threeBodyCircle = U (500 / earthD) 13.97e27 1000 bs (makeBarnes w bs) False
  where masses          = [5.97e24, 1.989e30, 1.989e30]
        distances       = [0.0e00, 4.5e10, (-4.5e10)]
        initXVelocities = [0.05e04, 3.0e04, (-3.0e04)]
        colors          = [G.blue, G.yellow, G.yellow]
        bs              = zipWith4 fx masses distances initXVelocities colors
        w               = 1.25e11

binaryStars = U (500 / earthD) 13.97e27 2000 bs (makeBarnes w bs) False
  where masses          = [1.5e30, 1.5e30]
        distances       = [4.5e10, (-4.5e10)]
        initXVelocities = [1.0e04, (-1.0e04)]
        colors          = [G.red, G.green]
        bs              = zipWith4 fx masses distances initXVelocities colors
        w               = 5.0e10

figureEight = U (500 / earthD) 13.97e27 500 bs (makeBarnes w bs) False
  where masses          = [1.989e30, 1.989e30, 1.989e30]
        distances       = [(9.7e10, -2.43e10), (-9.7e10, 2.43e10), (0, 0)]
        initXVelocities = [(3.66e04, 2.32e04), (3.66e04, 2.32e04), (-6.32e04, -5.64e04)]
        colors          = [G.blue, G.red, G.yellow]
        bs              = zipWith4 zipF masses distances initXVelocities colors
        w               = 1.25e11
        zipF m (x, y) (vx, vy) c = B m (P x y) (V vx vy) c []
