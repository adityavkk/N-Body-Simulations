{-# LANGUAGE FlexibleContexts #-}

module GalaxyModels where

import Test.QuickCheck
import DataTypes
import Utils
import Numeric.Tools.Integration
import Numeric.Tools.Differentiation
import GHC.Float
import qualified Graphics.Gloss as G

plummerRho :: Mass -> Radius -> Radius -> Float
plummerRho m a r = ((3 * m) / (4 * pi)) * (a^2 / (a^2 + r^2) ** (5/2))

hernquistRho :: Mass -> Radius -> Radius -> Float
hernquistRho m a r = (m / (2 * pi)) * (a / (r * (a + r)^3))

plummerPDF :: Mass -> Radius -> Radius -> Float
plummerPDF m a r = m * (r^3 / (r^2 + a^2) ** (3/2))

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
  -> (Mass -> Radius -> Radius -> Float)
  -> (Mass -> Radius -> Radius -> Float)
  -> Mass
  -> Radius
  -> [Body]
  -> Gen [Body]
vonNeumannSampling n rho pdf m rad bs
  | n == 0       = return bs
  | otherwise    = do
    r0 <- choose (0, 1) :: Gen Float
    p0 <- choose (0, 1) :: Gen Float
    let r = r0 * rad
        p = p0 * m
    p' <- randPos r
    let b = B (rho m rad r) p' (V 0 0) G.blue []
    if check r p
    then vonNeumannSampling (n - 1) rho pdf (m + mass b) rad (b:bs)
    else vonNeumannSampling n rho pdf m rad bs
  where
    check r p = pdf m rad r <= p

randPos :: Float -> Gen Pos
randPos r = do
  x <- choose (0, r)
  return (P x (sqrt (r^2 - x^2)))

f = generate $ vonNeumannSampling 50 plummerRho plummerPDF 1 1 []


