module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Gravity
import Bodies
import qualified DataTypes as T

type PixToKg = Float
type PixToMeter = Float

w   = 1500
off = 100
fps = 80 :: Int

window =
  InWindow "N-Body Simulation (Barnes Hut) by Aditya K." (w, w) (off, off)

render :: T.Universe -> Picture
render u = pictures $ draw u pToM pToKg <$> bs
  where bs    = T.bodies u
        pToM  = T.pixelToM u
        pToKg = T.pixelToKg u

draw :: T.Universe -> PixToMeter -> PixToKg -> T.Body -> Picture
draw u pToM pToKg (T.B m (T.P px py) _ c t) = pictures [circle, trail]
  where
    circle   = translate (pToM * px) ( pToM * py ) $ color c $ circleSolid 4
    trail    = color c $ line [(pToM * x, pToM * y) | (x, y) <- t]

move :: Float -> T.Universe -> T.Universe
move t u = moveUniv (T.simTimeRatio u * t) u

update = const move

handleKeys (EventKey (Char 't') Down _ _) u = u { T.trails = not $ T.trails u }
handleKeys _ u                              = u

main :: IO ()
main = play window black fps fourBodyStar render handleKeys move
