module Data.Stats where

data Stat =
  HP
  | Attack
  | Defence
  | SpAttack
  | SpDefence
  | Speed
  deriving (Eq,Show,Ord,Enum,Bounded)
