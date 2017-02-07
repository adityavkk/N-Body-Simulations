module DataTypes (
  Universe(..), Mass(..), Pos(..), Vel(..), Acc(..), Body(..)
) where

type Weight = Double
type Mass   = Double

data Pos = P { px :: Double
             , py :: Double
             } deriving (Show, Read, Eq)

data Vel = V { vx :: Double
             , vy :: Double
             } deriving (Show, Read, Eq)

data Acc = A { ax :: Double
             , ay :: Double
             } deriving (Show, Read, Eq)

data Body = B { mass :: Mass
             , pos   :: Pos
             , vel   :: Vel
             } deriving (Show, Read, Eq)

data Universe = U { pixelToM     :: Double
                  , pixelToKg    :: Double
                  , simTimeRatio :: Double
                  , bodies       :: [Body]
                  } deriving (Show, Read, Eq)

