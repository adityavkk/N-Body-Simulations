{-# LANGUAGE DuplicateRecordFields #-}

module DataTypes where

import qualified Graphics.Gloss as G

type Mass   = Float

data Pos = P { px :: !Float
             , py :: !Float
             } deriving (Show, Eq)

data Vel = V { vx :: !Float
             , vy :: !Float
             } deriving (Show, Eq)

data Acc = A { ax :: !Float
             , ay :: !Float
             } deriving (Show, Eq)

data Body = B { mass  :: !Mass
              , pos   :: !Pos
              , vel   :: !Vel
              , color :: !G.Color
              } deriving (Show, Eq)

data Universe = U { pixelToM     :: Float
                  , pixelToKg    :: !Float
                  , simTimeRatio :: !Float
                  , bodies       :: ![Body]
                  , barnesTree   :: !BarnesTree
                  } deriving (Show, Eq)

data BarnesLeaf = Leaf { blCenter :: !Pos
                       , blWidth  :: !Float
                       }
                | Node { bnCMass  :: !Pos
                       , bnCenter :: !Pos
                       , bnWidth  :: !Float
                       , blMass   :: !Mass
                       , body     :: !Body
                       } deriving (Show, Eq)

data BarnesTree = Exter !BarnesLeaf
                | Inter { cMass    :: !Pos
                        , btCenter :: !Pos
                        , width    :: !Float
                        , btMass   :: !Mass
                        , nw       :: !BarnesTree
                        , ne       :: !BarnesTree
                        , sw       :: !BarnesTree
                        , se       :: !BarnesTree
                        } deriving (Show, Eq)
