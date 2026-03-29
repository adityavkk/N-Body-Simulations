module SolarSystem
  ( solarSystem
  ) where

import DataTypes
import qualified Graphics.Gloss as G

-- | Reference values based on Earth
earthM :: Float
earthM = 5.9736e24

earthD :: Float
earthD = 152098232.0e3

earthV :: Float
earthV = 29.78e3

-- | The Sun
sun :: Body
sun = B 1.9891e30 (P 0 0) (V 0 0) G.yellow

-- | Planet data: mass multiplier, distance multiplier, velocity multiplier, color
planetData :: [(Float, Float, Float, G.Color)]
planetData =
  [ (0.0553 , 0.387 , 1.59  , G.greyN 0.5 )   -- Mercury
  , (0.815  , 0.723 , 1.18  , G.violet     )   -- Venus
  , (0.0123 , 1.003 , 1.034 , G.greyN 0.2  )   -- Moon
  , (1.0    , 1.0   , 1.0   , G.blue       )   -- Earth
  , (0.107  , 1.52  , 0.808 , G.red        )   -- Mars
  , (317.8  , 5.20  , 0.439 , G.orange     )   -- Jupiter
  , (95.2   , 9.58  , 0.325 , G.chartreuse )   -- Saturn
  , (14.5   , 19.20 , 0.228 , G.azure      )   -- Uranus
  , (17.1   , 30.05 , 0.182 , G.cyan       )   -- Neptune
  , (0.0025 , 39.48 , 0.157 , G.magenta    )   -- Pluto
  ]

-- | Build planet bodies from the data table
planets :: [Body]
planets = map mkPlanet planetData
  where
    mkPlanet (mMul, dMul, vMul, c) =
      B (earthM * mMul) (P (earthD * dMul) 0) (V 0 (earthV * vMul)) c

-- | Complete solar system universe with scaling factors
solarSystem :: Universe
solarSystem = U
  { pixelToM     = (125 * 0.4) / earthD
  , pixelToKg    = 13.97e27
  , simTimeRatio = 8000
  , bodies       = sun : planets
  }
