module GalaxyModels
  ( plummerRho
  , hernquistRho
  , plummerPDF
  , pdf
  , vonNeumannSampling
  ) where

import Test.QuickCheck
import DataTypes
import Integration (rombergIntegrate)
import GHC.Float
import qualified Graphics.Gloss as G

-- | Plummer sphere density profile
plummerRho :: Mass -> Radius -> Radius -> Float
plummerRho m a r = ((3 * m) / (4 * pi)) * (a ** 2 / (a ** 2 + r ** 2) ** (5/2))

-- | Hernquist sphere density profile
hernquistRho :: Mass -> Radius -> Radius -> Float
hernquistRho m a r = (m / (2 * pi)) * (a / (r * (a + r) ** 3))

-- | Plummer cumulative distribution (analytic)
plummerPDF :: Mass -> Radius -> Radius -> Float
plummerPDF m a r = m * (r ** 3 / (r ** 2 + a ** 2) ** (3/2))

-- | Numerical PDF via Romberg integration of the density profile
pdf :: (Mass -> Radius -> Float -> Float)
    -> Mass -> Radius -> Radius -> Float
pdf rho m r limit = double2Float $ rombergIntegrate 12 1e-8 rho' 0 d'
  where
    rho' :: Double -> Double
    rho' x = float2Double $ 4 * pi * x' ** 2 * rho m r x'
      where x' = double2Float x
    d' = float2Double limit

-- | Von Neumann rejection sampling to generate bodies from a density profile
vonNeumannSampling
  :: Int
  -> (Mass -> Radius -> Radius -> Float)
  -> (Mass -> Radius -> Radius -> Float)
  -> Mass
  -> Radius
  -> [Body]
  -> Gen [Body]
vonNeumannSampling n rho pdfFn m rad bs
  | n == 0    = return bs
  | otherwise = do
      r0 <- choose (0, 1) :: Gen Float
      p0 <- choose (0, 1) :: Gen Float
      let r = r0 * rad
          p = p0 * m
      p' <- randPos r
      let b = B (rho m rad r) p' (V 0 0) G.blue Nothing []
      if pdfFn m rad r <= p
        then vonNeumannSampling (n - 1) rho pdfFn (m + mass b) rad (b:bs)
        else vonNeumannSampling n rho pdfFn m rad bs

-- | Generate a random position at distance r from origin
randPos :: Float -> Gen Pos
randPos r = do
  x <- choose (0, r)
  return (P x (sqrt (r ** 2 - x ** 2)))
