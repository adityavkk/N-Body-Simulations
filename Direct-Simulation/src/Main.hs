module Main (main) where

import Graphics.Gloss

import Gravity
import SolarSystem
import qualified DataTypes as T

type PixToKg    = Float
type PixToMeter = Float

windowWidth :: Int
windowWidth = 1500

windowOffset :: Int
windowOffset = 100

fps :: Int
fps = 80

window :: Display
window =
  InWindow "N-Body Simulation (Direct Sim)" (windowWidth, windowWidth) (windowOffset, windowOffset)

render :: T.Universe -> Picture
render u = pictures $ draw pToM pToKg <$> bs
  where bs    = T.bodies u
        pToM  = T.pixelToM u
        pToKg = T.pixelToKg u

draw :: PixToMeter -> PixToKg -> T.Body -> Picture
draw pToM _pToKg (T.B _m (T.P bx by) _ c) =
  translate (pToM * bx) (pToM * by) $ color c $ circleSolid 4

move :: Float -> T.Universe -> T.Universe
move t u = moveUniv (T.simTimeRatio u * t) u

update :: a -> Float -> T.Universe -> T.Universe
update = const move

main :: IO ()
main = simulate window black fps solarSystem render update
