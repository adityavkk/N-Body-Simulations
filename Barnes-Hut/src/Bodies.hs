{-# LANGUAGE FlexibleContexts #-}
module Bodies where

import DataTypes
import Gravity
import BarnesHut
import Data.List hiding (insert)
import qualified Graphics.Gloss as G
import Test.QuickCheck

massGen = oneof [choose (0, 2), choose (2, 4), choose (50, 100)]  :: Gen Float
distGen = oneof [choose (-1, 0), choose (-10, -1), choose (-40, -20),choose (0, 1), choose (1, 10), choose (20, 40)] :: Gen Float

genDistVel :: Gen (Float, Float)
genDistVel = distGen >>= f
  where f n = return (n, (1 / (sqrt $ abs n)) :: Float)

rP :: Gen (Float, Float, Float, Float)
rP = do
  (dist, vel) <- genDistVel
  mass <- massGen
  d' <- distGen
  return (mass, dist, vel, d')

rPs :: Int -> Gen [(Float, Float, Float, Float)]
rPs n = vectorOf n rP

bs n = rPs n >>=
  (\ ps -> return $ map (\ (m, d, v, y) -> B (earthM * m) (P (earthD * d) (earthD * y)) (V 0 (earthV * v)) G.azure) ps)

crazyU :: Int -> Gen Universe
crazyU n = bs n >>=
  (\ ps ->
    return $ U ((125 * 0.4) / 152098232.0e3) 13.97e27 3000 (jupiter:sun:ps) (makeBarnes 20e12 (jupiter:sun:ps)))
  where jupiter = planets !! 5

sun      = B 1.9891e30 (P (-1) (-1)) (V 0 0) G.yellow
earthM   = 5.9736e24
earthD   = 152098232.0e3
earthV   =  29.78e3
masses =
  map (* earthM)
    [0.0553, 0.815, 0.0123, 1.0, 0.107, 317000.8, 95.2, 14.5, 17.1, 0.0025]
distances =
  map (* earthD)
    [0.387, 0.723, 1.00257, 1.0, 1.52, 2.20, 9.58, 19.20, 30.05, 39.48]
initVelocities =
  map (* earthV)
    [1.59, 1.18, 1.0343, 1.0, 0.808, 0.439, 0.325, 0.228, 0.182, 0.157]
colors = [G.greyN 0.5, G.violet, G.greyN 0.2, G.blue, G.red, G.orange, G.chartreuse, G.azure, G.cyan, G.magenta]

planets = zipWith4 fy masses distances initVelocities colors

fy m d v = B m (P d 1) (V 0 v)
fx m d v = B m (P 1 d) (V v 0)

solarSystem = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:planets)
                (makeBarnes w (sun:planets))
  where w = 20e12

doublePs n ps = foldr
  (\ p@(B m (P x y) (V vx vy) _) ps -> p {pos = P (x / n) y } : p : ps)
                 [] ps

lotaPlanets n = last $ take n $ iterate (doublePs 2) planets

lotaSystem = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:lop)
                (makeBarnes w (sun:lop))
  where w = 20e12
        lop = lotaPlanets 10

fourBodyStar = U (500 / earthD) 13.97e27 1500 bs (makeBarnes w bs)
  where masses         = [3.0e28, 3.0e28, 3.0e28, 3.0e28]
        distances      = [-3.5e10, -1.0e10, 1.0e10, 3.5e10]
        initVelocities = [1.4e03, 1.4e04, (-1.4e04), (-1.4e03)]
        colors         = repeat G.violet
        bs             = zipWith4 fy masses distances initVelocities colors
        w              = 5.0e10

threeBodyCircle = U (500 / earthD) 13.97e27 1000 bs (makeBarnes w bs)
  where masses          = [5.97e24, 1.989e30, 1.989e30]
        distances       = [0.0e00, 4.5e10, (-4.5e10)]
        initXVelocities = [0.05e04, 3.0e04, (-3.0e04)]
        colors          = repeat G.violet
        bs              = zipWith4 fx masses distances initXVelocities colors
        w               = 1.25e11

binaryStars = U (500 / earthD) 13.97e27 1500 bs (makeBarnes w bs)
  where masses          = [1.5e30, 1.5e30]
        distances       = [4.5e10, (-4.5e10)]
        initXVelocities = [1.0e04, (-1.0e04)]
        colors          = repeat G.magenta
        bs              = zipWith4 fx masses distances initXVelocities colors
        w               = 5.0e10
