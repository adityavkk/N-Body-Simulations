module Render where

import qualified DataTypes as T
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Gravity
import Bodies
import Data.Maybe

type PixToKg = Float
type PixToMeter = Float

w   = 1500
off = 100
fps = 80 :: Int

initState :: T.Universe -> T.Rendering
initState u = T.Render u False False False False False

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
handleKeys (EventKey (Char 'f') s _ _) r
  | s == Down = r { T.fast = True  }
  | otherwise = r { T.fast = False }
handleKeys (EventKey (Char 's') s _ _) r
  | s == Down = r { T.slow = True  }
  | otherwise = r { T.slow = False }
handleKeys (EventKey (Char '1') Down _ _) r =
  r { T.universe = binaryStars }
handleKeys (EventKey (Char '2') Down _ _) r =
  r { T.universe = threeBodyCircle }
handleKeys (EventKey (Char '3') Down _ _) r =
  r { T.universe = fourBodyStar }
handleKeys (EventKey (Char '4') Down _ _) r =
  r { T.universe = solarSystem }
handleKeys _ r                              = r

render :: T.Rendering -> Picture
render r = pictures $ draw u pToM pToKg <$> bs
  where u     = T.universe r
        bs    = T.bodies u
        pToM  = T.pixelToM u
        pToKg = T.pixelToKg u

draw :: T.Universe -> PixToMeter -> PixToKg -> T.Body -> Picture
draw u pToM pToKg (T.B m (T.P px py) _ c s t) = pictures [circle, trail]
  where
    circle   = if isNothing s
               then translate (pToM * px) ( pToM * py ) $ color c $ circleSolid 10
               else translate (pToM * px) ( pToM * py ) $ color c $ circleSolid (pToKg * fromJust s)
    trail    = color c $ line [(pToM * x, pToM * y) | (x, y) <- t]

move :: Float -> T.Rendering -> T.Rendering
move t r
  | zoomOut && not paused =
    r { T.universe = moveU { T.pixelToM = T.pixelToM moveU * 0.99 }}
  | zoomIn && not paused  =
    r { T.universe = moveU { T.pixelToM = T.pixelToM moveU * 1.01 }}
  | zoomOut               =
    r { T.universe = u { T.pixelToM = T.pixelToM u * 0.99 }}
  | zoomIn                =
    r { T.universe = u { T.pixelToM = T.pixelToM u * 1.01 }}
  | fst && not paused     =
    r { T.universe = moveU { T.simTimeRatio = T.simTimeRatio moveU * 1.005 }}
  | slw && not paused     =
    r { T.universe = moveU { T.simTimeRatio = T.simTimeRatio moveU * 0.995 }}
  | paused                = r
  | otherwise             = r { T.universe = moveU }
  where u       = T.universe r
        zoomOut = T.zOut r
        zoomIn  = T.zIn r
        moveU   = moveUniv (T.simTimeRatio u * t) u
        paused  = T.paused r
        fst      = T.fast r
        slw      = T.slow r

update :: b -> Float -> T.Rendering -> T.Rendering
update = const move

window :: Display
window =
  InWindow "N-Body Simulation (Barnes Hut) by Aditya K." (w, w) (off, off)

simulate :: IO ()
simulate = play window black fps (initState binaryStars) render handleKeys move
