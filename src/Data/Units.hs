module Data.Units where

import Control.Monad
import Control.Applicative

newtype Kg = Kg { unKg :: Double } deriving (Eq,Show,Ord)

newtype Meters = Meters { unMeters :: Double } deriving (Eq,Show,Ord)

kg :: (Alternative f, Monad f) => Double -> f Kg
kg d = do
  guard $ d < unKg maxBound && d > unKg minBound
  pure . Kg $ d

meters :: (Alternative f, Monad f) => Double -> f Meters
meters d = do
  guard $ d < unMeters maxBound && d > unMeters minBound
  pure . Meters $ d

instance Bounded Kg where
  minBound = Kg 0
  maxBound = Kg 1000 -- Maximum Pokemon weight is 999.9 kg in the games (Cosmoem and Celesteela)

instance Bounded Meters where
  minBound = Meters 0
  maxBound = Meters 100 -- Maximum Pokemon height is 100 m in the games (Eternamax Eternatus)
