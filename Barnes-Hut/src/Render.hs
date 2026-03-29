module Render
  ( simulate
  ) where

import qualified DataTypes as T
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Gravity (moveUniv)
import Bodies
import Data.Maybe (isNothing, fromJust)
import Test.QuickCheck (generate)

type PixToKg    = Float
type PixToMeter = Float

windowWidth :: Int
windowWidth = 1500

windowOffset :: Int
windowOffset = 100

fps :: Int
fps = 80

initState :: T.Universe -> T.Rendering
initState u = T.Render u False False False False False

render :: T.Rendering -> Picture
render r = pictures $ draw u pToM pToKg <$> bs
  where u    = T.universe r
        bs   = T.bodies u
        pToM = T.pixelToM u
        pToKg = T.pixelToKg u

draw :: T.Universe -> PixToMeter -> PixToKg -> T.Body -> Picture
draw _u pToM pToKg (T.B _m (T.P bx by) _ c s t) = pictures [circle', trail']
  where
    circle'
      | isNothing s =
          translate (pToM * bx) (pToM * by) $ color c $ circleSolid 7
      | otherwise =
          translate (pToM * bx) (pToM * by) $ color c $ circleSolid (pToKg * fromJust s)
    trail' = color c $ line [(pToM * x, pToM * y) | (x, y) <- t]

move :: Float -> T.Rendering -> T.Rendering
move t r
  | zoomOut && not isPaused =
      r { T.universe = moveU { T.pixelToM = T.pixelToM moveU * 0.99 }}
  | zoomIn && not isPaused =
      r { T.universe = moveU { T.pixelToM = T.pixelToM moveU * 1.01 }}
  | zoomOut =
      r { T.universe = u { T.pixelToM = T.pixelToM u * 0.99 }}
  | zoomIn =
      r { T.universe = u { T.pixelToM = T.pixelToM u * 1.01 }}
  | isFast && not isPaused =
      r { T.universe = moveU { T.simTimeRatio = T.simTimeRatio moveU * 1.005 }}
  | isSlow && not isPaused =
      r { T.universe = moveU { T.simTimeRatio = T.simTimeRatio moveU * 0.995 }}
  | isPaused = r
  | otherwise = r { T.universe = moveU }
  where u        = T.universe r
        zoomOut  = T.zOut r
        zoomIn   = T.zIn r
        moveU    = moveUniv (T.simTimeRatio u * t) u
        isPaused = T.paused r
        isFast   = T.fast r
        isSlow   = T.slow r

window :: Display
window =
  InWindow "N-Body Simulation (Barnes-Hut)" (windowWidth, windowWidth) (windowOffset, windowOffset)

-- | Main simulation entry point with keyboard controls
simulate :: IO ()
simulate = do
  rendering <- generate (initState <$> crazyU 3750)
  let
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
    handleKeys (EventKey (Char '5') Down _ _) r =
      r { T.universe = T.universe rendering }
    handleKeys (EventKey (Char '6') Down _ _) r =
      r { T.universe = figureEight }
    handleKeys _ r = r

  play window black fps (initState binaryStars) render handleKeys move
