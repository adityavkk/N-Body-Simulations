{-# LANGUAGE FlexibleContexts #-}

module GalaxyModels where

import Test.QuickCheck
import DataTypes
import Utils
import Numeric.Tools.Integration
import Numeric.Tools.Differentiation
import GHC.Float
import qualified Graphics.Gloss as G
import BarnesHut hiding (insert)

solarMass = 1.9891e30
solarLength = 20e12
scaleFactor = (3.0 * pi) / 16.0
earthDiam   = 152098232.0e3

plummerRho :: Mass -> Radius -> Radius -> Float
plummerRho m a r = ((3 * m) / (4 * pi)) * (a^2 / (a^2 + r^2) ** (5/2))

hernquistRho :: Mass -> Radius -> Radius -> Float
hernquistRho m a r = (m / (2 * pi)) * (a / (r * (a + r)^3))

plummerPDF :: Mass -> Radius -> Radius -> Float
plummerPDF m a r = m * (r^3 / (r^2 + a^2) ** (3/2))

plummerVPDF :: Radius -> Radius -> Float
plummerVPDF a r = undefined

pdf ::
  (Mass -> Radius -> Float -> Float)
  -> Mass
  -> Radius
  -> Radius
  -> Float
pdf rho m r d = double2Float $ quadBestEst $ quadRomberg defQuad (0, d') rho'
  where rho' :: Double -> Double
        rho' x = float2Double $ 4 * pi * x'^2 * rho m r x'
          where x' = double2Float x

        d'     = float2Double d

vonNeumannSampling ::
  Int
  -> Float
  -> (Mass -> Radius -> Radius -> Float)
  -> (Mass -> Radius -> Radius -> Float)
  -> Mass
  -> Radius
  -> [Body]
  -> Gen [Body]
vonNeumannSampling n m0 rho pdf m rad bs
  | n == 0       = return bs
  | otherwise    = do
    r0 <- choose (0, 1) :: Gen Float
    p0 <- choose (0, 1) :: Gen Float
    let r = r0 * rad
        p = p0 * m
    p' <- randPos r
    v  <- velGen r
    let b = B m0 p' v G.blue []
    if check r p
    then vonNeumannSampling (n - 1) m0 rho pdf m rad (b:bs)
    else vonNeumannSampling n m0 rho pdf m rad bs
  where
    check r p = pdf m rad r <= p

velGen :: Radius -> Gen Vel
velGen r = do
  x <- choose (0, 1)   :: Gen Float
  y <- choose (0, 0.1) :: Gen Float
  if y > x^2 * (1 - x^2) ** 3.5
  then velGen r
  else randVel (x * sqrt 2.0 * (1.0 + r^2) ** (-0.25))

randPos :: Float -> Gen Pos
randPos r = do
  x <- choose (0, r)
  return (P x (sqrt (r^2 - x^2)))

randVel :: Float -> Gen Vel
randVel v = do
  x <- choose (0, v)
  return (V x (sqrt (v^2 - x^2)))

plummerSphere :: Int -> Mass -> Radius -> Gen Universe
plummerSphere n mSi rSi = do
  bs <- vonNeumannSampling n (1/ fromIntegral n) plummerRho plummerPDF 1 1 []
  let bs' = map scalePosVel bs
      u = U 1.0e2 13.97e27 1.0e-3 mSi bs' (makeBarnes 2 bs') rSi False
  return u
  where
    scalePosVel b = b { pos = scalePos (pos b) scaleFactor scaleFactor
                      , vel = scaleVel (vel b) (sqrt scaleFactor) (sqrt scaleFactor)
                      }

f' = generate $ vonNeumannSampling 50 (1/50) plummerRho plummerPDF 1 1 []
g = generate $ plummerSphere 50 1 1
