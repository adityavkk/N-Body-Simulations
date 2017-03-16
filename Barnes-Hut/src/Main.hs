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

window :: Display
window =
  InWindow "N-Body Simulation (Barnes Hut) by Aditya K." (w, w) (off, off)

render :: T.Rendering -> Picture
render r = pictures $ draw u pToM pToKg <$> bs
  where u     = T.universe r
        bs    = T.bodies u
        pToM  = T.pixelToM u
        pToKg = T.pixelToKg u

draw :: T.Universe -> PixToMeter -> PixToKg -> T.Body -> Picture
draw u pToM pToKg (T.B m (T.P px py) _ c t) = pictures [circle, trail]
  where
    circle   = translate (pToM * px) ( pToM * py ) $ color c $ circleSolid 4
    trail    = color c $ line [(pToM * x, pToM * y) | (x, y) <- t]

move :: Float -> T.Rendering -> T.Rendering
move t r
  | zoomOut   = r { T.universe = moveU { T.pixelToM = T.pixelToM moveU * 0.99 }}
  | zoomIn    = r { T.universe = moveU { T.pixelToM = T.pixelToM moveU * 1.01 }}
  | paused    = r
  | otherwise = r { T.universe = moveU }
  where u       = T.universe r
        zoomOut = T.zOut r
        zoomIn  = T.zIn r
        moveU   = moveUniv (T.simTimeRatio u * t) u
        paused  = T.paused r

update :: b -> Float -> T.Rendering -> T.Rendering
update = const move

initState :: T.Rendering
initState = T.Render fourBodySystem False False False

handleKeys :: Event -> T.Rendering -> T.Rendering
handleKeys (EventKey (Char 't') Down _ _) r =
  r { T.universe = u { T.trails = not $ T.trails u } }
    where u = T.universe r
handleKeys (EventKey (Char 'p') Down _ _) r =
  r { T.paused = not $ T.paused r }
handleKeys (EventKey (Char '=') s _ _) r
  | s == Down = r { T.zIn = True }
  | otherwise = r { T.zIn = False }
handleKeys (EventKey (Char '-') s _ _) r
  | s == Down = r { T.zOut = True }
  | otherwise = r { T.zOut = False }
handleKeys _ r                              = r

main :: IO ()
main = play window black fps initState render handleKeys move
