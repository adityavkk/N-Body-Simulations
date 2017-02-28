module Bodies where

import DataTypes
import Gravity
import BarnesHut
import Data.List hiding (insert)
import qualified Graphics.Gloss as G

sun      = B 1.9891e30 (P (-1) (-1)) (V 0 0) G.yellow
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

planets = zipWith4 f masses distances initVelocities colors
  where f m d v c = B m (P d 1) (V 0 v) c

solarSystem w = U ((125 * 0.4) / 152098232.0e3) 13.97e27 2000
                (sun:planets)
                (makeBarnes w (sun:planets))

sunMerc w = U ((125 * 0.4) / 152098232.0e3) 13.97e27 1000
                (sun:(take 1 $ reverse planets))
                (makeBarnes w (sun:(take 1 $ reverse planets)))
