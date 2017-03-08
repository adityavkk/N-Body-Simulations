module Main where

import Graphics.Gloss
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
render u = pictures $ draw pToM pToKg <$> bs
  where bs    = T.bodies u
        pToM  = T.pixelToM u
        pToKg = T.pixelToKg u

draw :: PixToMeter -> PixToKg -> T.Body -> Picture
draw pToM pToKg (T.B m (T.P px py) _ c t) = pictures [circle, trail]
  where
    circle   = translate (pToM * px) ( pToM * py ) $ color c $ circleSolid 4
    trail    = pictures $ map (\ (a, b) -> (color c . line) [a, b]) 
                              (zip (init trailPts) (tail trailPts))
    trailPts = reverse $ foldr (\ (x, y) pts -> (pToM * x, pToM * y) : pts) [] (T.lru t)

move :: Float -> T.Universe -> T.Universe
move t u = moveUniv (T.simTimeRatio u * t) u

update = const move

main :: IO ()
main = simulate window black fps fourBodyStar render update
