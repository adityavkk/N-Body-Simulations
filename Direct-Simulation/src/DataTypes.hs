module DataTypes (
  Universe(..), Mass(..), Pos(..), Vel(..), Acc(..), Body(..)
) where

import qualified Graphics.Gloss as G

type Mass   = Float

data Pos = P { px :: Float
             , py :: Float
             } deriving (Show, Eq)

data Vel = V { vx :: Float
             , vy :: Float
             } deriving (Show, Eq)

data Acc = A { ax :: Float
             , ay :: Float
             } deriving (Show, Eq)

data Body = B { mass :: Mass
             , pos   :: Pos
             , vel   :: Vel
             , color :: G.Color
             } deriving (Show, Eq)

data Universe = U { pixelToM     :: Float
                  , pixelToKg    :: Float
                  , simTimeRatio :: Float
                  , bodies       :: [Body]
                  } deriving (Show, Eq)

