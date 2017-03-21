module RandomBodies where

import Test.QuickCheck
import DataTypes
import Utils
import Numeric.Tools.Integration
import Numeric.Tools.Differentiation

plummerRho :: Mass -> Radius -> Float -> Float
plummerRho m a r = ((3 * m) / (4 * pi)) * (a^2 / (a^2 + r^2) ** (5/2))

hernquistRho :: Mass -> Radius -> Float -> Float
hernquistRho m a r = (m / (2 * pi)) * (a / (r * (a + r)^3))

plummerPDF :: Mass -> Radius -> Pos -> Float
plummerPDF m a p = m * (r^3 / (r^2 + a^2) ** (3/2))
  where r = d (P 0 0) p

pdf ::
  Radius
  -> (Mass -> Radius -> Pos -> Float)
  -> Mass
  -> Radius
  -> Pos
  -> Float
  -> Float
pdf d rho m r p = quadBestEst $ quadRomberg defQuad (0, d) rho'
  where rho' :: Double -> Double
        rho' x = rho m r 

vonNeumannSampling ::
  Int
  -> (Mass -> Radius -> Pos -> Float)
  -> Mass -> Radius
  -> [Pos]
  -> Gen [Pos]
vonNeumannSampling n pdf m rad ps
  | n == 0       = return ps
  | otherwise    = do
    r0 <- choose (0, 1) :: Gen Float
    p0 <- choose (0, 1) :: Gen Float
    let r = r0 * rad
        p = p0 * m
    p' <- randPos r
    if check r p p'
    then vonNeumannSampling (n - 1) pdf m rad (p':ps)
    else vonNeumannSampling n pdf m rad ps
  where
    check r p p' = pdf m rad p' <= p
    randPos r = do
      x <- choose (0, r)
      return (P x (sqrt (r^2 - x^2)))

f = generate $ vonNeumannSampling 5 plummerPDF 1 1 []
