module Bodies where

import DataTypes
import Gravity
import BarnesHut hiding (insert)
import Data.List hiding (insert)
import qualified Graphics.Gloss as G

sun      = B 1.9891e30 (P (-1) (-1)) (V 0 0) G.yellow []
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

fy m d v c = B m (P d 1) (V 0 v) c []
fx m d v c = B m (P 1 d) (V v 0) c []

solarSystem = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:planets)
                (makeBarnes w (sun:planets))
                False
  where w = 20e12

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
