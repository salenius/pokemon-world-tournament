module PWT.Trainer where

type Fighter pkmn mv it = (pkmn, Individual mv it)

trainer :: Name -> Class -> Party (Fighter pkmn mv it) -> Trainer pkmn mv it

individual :: MoveSet mv -> Level -> Individual mv it

withItem :: Individual mv it -> it -> Individual mv it
iv, ev :: Stat -> Int -> Individual mv it -> Individual mv it
nature :: Nature -> Individual mv it -> Individual mv it
withHP :: (MaxHP -> HP) -> Individual mv it -> Individual mv it
withStatus :: Status -> Individual mv it -> Individual mv it

filterParty :: (Fighter pkmn mv it -> Bool) -> Trainer pkmn mv it -> Trainer pkmn mv it

withItemBag :: Trainer pkmn mv it -> [it] -> Trainer pkmn mv it

withReward :: Trainer pkmn mv it -> Reward -> Trainer pkmn mv it

tmap :: Trainer pkmn mv it -> TMap


instance Bifunctor Individual where
  bimap = undefined
