module SolarSystem where

import DataTypes
import Gravity

sun     = B 1.9891e30 (P 0 0) (V 0 0)
mercury = B 3.3022e23 (P 4.6e10 0) (V 0 49.88e3)
venus   = B 4.869e24 (P 1.08e11 0) (V 0 35.0e3)
earth   = B 5.9736e24 (P 152098232.0e3 0) (V 0 29.78e3)

solarSystem = U ((750.0 * 4) / 152098232.0e3) (5.9736e24 / 10000) 500
                [sun, mercury, venus, earth]
