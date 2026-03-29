-- | Simple numerical integration to replace the abandoned numeric-tools package.
module Integration
  ( rombergIntegrate
  ) where

-- | Romberg integration of a function over [a, b].
--   Uses Richardson extrapolation on the trapezoidal rule.
--   Iterates up to @maxIter@ levels or until relative error < @tol@.
rombergIntegrate :: Int -> Double -> (Double -> Double) -> Double -> Double -> Double
rombergIntegrate maxIter tol f a b = go 1 [trap0] []
  where
    trap0 = 0.5 * (b - a) * (f a + f b)

    go :: Int -> [Double] -> [Double] -> Double
    go n prev _
      | n > maxIter = head prev
    go n prev _old =
      let -- Refine trapezoidal estimate
          numNew = 2 ^ (n - 1) :: Int
          h      = (b - a) / fromIntegral (2 ^ n :: Int)
          sumNew = sum [f (a + fromIntegral (2 * k - 1) * h) | k <- [1..numNew]]
          trapN  = head prev / 2 + h * sumNew

          -- Richardson extrapolation
          curr   = buildRow n [trapN] (head prev : tail prev)
      in if n >= 2 && abs (head curr - head (tail curr)) < tol * abs (head curr)
         then head curr
         else go (n + 1) curr prev

    buildRow :: Int -> [Double] -> [Double] -> [Double]
    buildRow _ acc [] = reverse acc
    buildRow _ acc [_] = reverse acc
    buildRow level acc (r0:rest@(_:_)) =
      let prevNew = head acc
          factor  = 4 ^ (length acc) :: Int
          next    = prevNew + (prevNew - r0) / fromIntegral (factor - 1)
      in buildRow level (next : acc) rest
