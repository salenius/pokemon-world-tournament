{-# LANGUAGE FlexibleInstances #-}
module Party.Gender where

import System.Random

data Gender a = Male | Female deriving (Eq,Ord,Enum,Bounded,Show)

data SevenOutOfEight = SevenOutOfEight deriving (Eq,Ord,Enum,Bounded,Show)

data ThreeOutOfFour = ThreeOutFour deriving (Eq,Ord,Enum,Bounded,Show)

instance Random (Gender Bool) where
  randomR (a,b) g = (\(x,y) -> (outBool x, y)) $ randomR (asBool a, asBool b) g
    where
      asBool Male   = False
      asBool Female = True
      outBool False = Male
      outBool True  = Female
  random g = randomR (minBound,maxBound) g

instance Random (Gender SevenOutOfEight) where
  randomR (a,b) g = f . randomR (h a,h b) $ g
    where
      f (x,y)
        | x <= 875 = (Male :: Gender SevenOutOfEight, y)
        | otherwise  = (Female :: Gender SevenOutOfEight, y)
      h :: Gender SevenOutOfEight -> Float
      h Male   = 0
      h Female = 1000
  random g = randomR (minBound,maxBound) g

instance Random (Gender ThreeOutOfFour) where
  randomR (a,b) g = f . randomR (h a,h b) $ g
    where
      f (x,y)
        | x <= 75 = (Male :: Gender ThreeOutOfFour, y)
        | otherwise  = (Female :: Gender ThreeOutOfFour, y)
      h :: Gender ThreeOutOfFour -> Float
      h Male   = 0
      h Female = 100
  random g = randomR (minBound,maxBound) g
