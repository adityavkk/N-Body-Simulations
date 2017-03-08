module Bodies where

import DataTypes
import Gravity
import BarnesHut hiding (insert)
import Data.List hiding (insert)
import qualified Graphics.Gloss as G
import Data.LruCache

mtLRU    = debouncedMt 0 250
sun      = B 1.9891e30 (P (-1) (-1)) (V 0 0) G.yellow (forceInsert (-1) (-1, -1) mtLRU)
earthM   = 5.9736e24
earthD   = 152098232.0e3
earthV   =  29.78e3
masses =
  map (* earthM)
    [0.0553, 0.815, 0.0123, 1.0, 0.107, 317.8, 95.2, 14.5, 17.1, 0.0025]
distances =
  map (* earthD)
    [0.387, 0.723, 1.00257, 1.0, 1.52, 5.20, 9.58, 19.20, 30.05, 39.48]
initVelocities =
  map (* earthV)
    [1.59, 1.18, 1.0343, 1.0, 0.808, 0.439, 0.325, 0.228, 0.182, 0.157]
colors = [G.greyN 0.5, G.violet, G.greyN 0.2, G.blue, G.red, G.orange, G.chartreuse, G.azure, G.cyan, G.magenta]

planets = zipWith4 fy masses distances initVelocities colors

fy m d v c = B m (P d 1) (V 0 v) c (forceInsert d (d, 1) mtLRU)
fx m d v c = B m (P 1 d) (V v 0) c (forceInsert d (1, d) mtLRU)

solarSystem = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:planets)
                (makeBarnes w (sun:planets))
  where w = 20e12

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
