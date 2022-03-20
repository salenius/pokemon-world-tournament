{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Logic.Damage (
  damage,
  DamageStats,
  LevelCalc,
  BasepowerCalc,
  CriticalHitCalc,
  Modifier,
  DamageCalc,
  Damage,
  Basepower,
  mkBasepower,
  mkLevelCalc,
  mkModifier,
  mkModifierWithCritical,
  mkCriticalHitCalc,
  mkDamageStats,
  basepower
  
                    ) where

import Logic.Damage.Ratio

damage ::
  LevelCalc battle
  -> DamageStats battle mv
  -> BasepowerCalc battle
  -> Ratio battle
  -> Modifier battle
  -> DamageCalc battle mv
damage lvl dam bp rat mod = \btl mv ->
  mod (getCrit dam btl) btl $
  levelCalc lvl lvlConst btl *
  ratioCalc rat (getCrit dam btl) (getCategory dam mv) btl *
  bp btl (getBasepower dam mv)

type DamageStats battle mv = (battle -> CriticalHit, mv -> Category, mv -> Basepower) 

type LevelCalc battle = Counterparty -> battle -> Int

type BasepowerCalc battle = battle -> Basepower -> Double

type Modifier battle = CriticalHit -> battle -> Double -> Double

type CriticalHitCalc battle = battle -> CriticalHit

type DamageCalc battle mv = battle -> mv -> Damage

type Damage = Double

type Basepower = Int

instance {-# OVERLAPS #-} Semigroup (Modifier battle) where
  f <> g = \cr b d -> g cr b . f cr b $ d

mkBasepower :: (battle -> Basepower -> Double) -> BasepowerCalc battle
mkBasepower = id

mkLevelCalc :: (Counterparty -> battle -> Int) -> LevelCalc battle
mkLevelCalc = id

mkCriticalHitCalc :: (battle -> CriticalHit) -> CriticalHitCalc battle
mkCriticalHitCalc = id

mkModifier :: (battle -> a) -> (a -> Double -> Double) -> Modifier battle
mkModifier f g = \_ b d -> (flip g) d . f $ b

mkModifierWithCritical ::
  (CriticalHit -> battle -> a)
  -> (a -> Double -> Double)
  -> Modifier battle
mkModifierWithCritical f g = \cr b d -> (flip g) d . f cr $ b

mkDamageStats ::
  (battle -> CriticalHit)
  -> (mv -> Category)
  -> (mv -> Basepower)
  -> DamageStats battle mv
mkDamageStats = (,,)

basepower :: BasepowerCalc battle
basepower = \_ bp -> fromIntegral bp


-----

getCrit :: DamageStats battle mv -> (battle -> CriticalHit)
getCrit (x,y,z) = x

getCategory :: DamageStats battle mv -> (mv -> Category)
getCategory (x,y,z) = y

getBasepower :: DamageStats battle mv -> (mv -> Basepower)
getBasepower (x,y,z) = z

levelCalc :: LevelCalc battle -> Double -> battle -> Double
levelCalc lvl mlt btl = mlt * (2 + 2 * fromIntegral (lvl User btl) / 5)

ratioCalc rat cr cat btl = fst (rat cr cat btl) / snd (rat cr cat btl)

lvlConst = 1/50

