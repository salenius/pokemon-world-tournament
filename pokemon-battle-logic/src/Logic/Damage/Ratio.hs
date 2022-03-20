module Logic.Damage.Ratio (
  ratio
  ,attackStat
  ,defenceStat
  ,attackModif
  ,defenceModif
  ,baseAttack
  ,baseDefence
  ,UpOrDown(..)
  ,Ratio
  ,statFrom
  ,flipCategory
  ,swapStat
  ,toMult
  ,module Logic.Damage.Attribute
                          ) where

import Logic.Damage.Attribute

data UpOrDown = Down | Up deriving (Eq,Show,Ord,Enum,Bounded)

type Ratio a = CriticalHit -> Category -> a -> (Double,Double)

type DefenceStat battle = CriticalHit -> Category -> battle -> Double

type AttackStat battle = DefenceStat battle

type BaseAttackStat battle = Counterparty -> Category -> battle -> Double

type BaseDefenceStat battle = BaseAttackStat battle

type AttackModifier battle = Counterparty -> CriticalHit -> Category -> battle -> Double

type DefenceModifier battle = AttackModifier battle


ratio :: AttackStat battle -> DefenceStat battle -> Ratio battle
ratio at def = tuple at def

attackStat :: BaseAttackStat battle -> AttackModifier battle -> AttackStat battle
attackStat = mainStat (fractionParty Up)

defenceStat :: BaseDefenceStat battle -> DefenceModifier battle -> DefenceStat battle
defenceStat = mainStat (fractionParty Down)

attackModif ::
  (Counterparty -> Category -> battle -> Int)
  -> (CriticalHit -> Int -> Int)
  -> AttackModifier battle
attackModif f g = \cp cr cat -> toMult 2 . g cr . f cp cat

defenceModif ::
  (Counterparty -> Category -> battle -> Int)
  -> (CriticalHit -> Int -> Int)
  -> DefenceModifier battle
defenceModif = attackModif

baseAttack :: (Counterparty -> Category -> battle -> Double) -> BaseAttackStat battle
baseAttack = id

baseDefence :: (Counterparty -> Category -> battle -> Double) -> BaseDefenceStat battle
baseDefence = id

statFrom :: Counterparty -> (Counterparty -> a) -> (Counterparty -> a)
statFrom cp f = \_ -> f cp

flipCategory :: (Category -> a) -> (Category -> a)
flipCategory = undefined

swapStat :: (Stat -> Stat) -> (Category -> a) -> (Category -> a)
swapStat = undefined

---

tuple f g = \h i j -> (f h i j, g h i j)

fractionParty Up = User
fractionParty Down = Target

-- mainStat :: Counterparty -> BaseAttackStat battle -> AttackModifier battle -> AttackStat battle
mainStat cp stat modif = \crit cat btl -> stat cp cat btl * modif cp crit cat btl

toMult :: Int -> Int -> Double
toMult beg = toDiv . flooring . asMult (beg,beg)
  where
    asMult (x,y) z
      | z > 0 = asMult (x+1,y) (z -1)
      | z < 0 = asMult (x,y+1) (z + 1)
      | otherwise = (x,y)
    toDiv = uncurry (/) 
    flooring (x,y) = (fromIntegral x, fromIntegral y)
