{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.Units where

import Control.Monad
import Control.Applicative

newtype Kg = Kg { unKg :: Double } deriving (Eq,Show,Ord,Num)

newtype Meters = Meters { unMeters :: Double } deriving (Eq,Show,Ord,Num)

oneKg :: Kg
oneKg = Kg 1

oneMeter :: Meters
oneMeter = Meters 1

kg :: Double -> Kg
kg = Kg

meters :: Double -> Meters
meters = Meters

instance Bounded Kg where
  minBound = Kg 0
  maxBound = Kg 1000 -- Maximum Pokemon weight is 999.9 kg in the games (Cosmoem and Celesteela)

instance Bounded Meters where
  minBound = Meters 0
  maxBound = Meters 100 -- Maximum Pokemon height is 100 m in the games (Eternamax Eternatus)
