module Champions.Item where

import Champions.Pokemon

data Item =
  AirBalloon
  | ChartiBerry
  | ChoiceBand
  | ChoiceScarf
  | ExpertBelt
  | FocusSash
  | KingsRock
  | Leftovers
  | LifeOrb
  | LightBall
  | OccaBerry
  | PowerHerb
  | QuickClaw
  | RindoBerry
  | RockyHelmet
  | ScopeLens
  | SitrusBerry
  | WhiteHerb
  deriving (Eq,Show,Ord,Enum,Bounded)

class HeldItem pkmn where
  heldItem :: pkmn -> Item

instance HeldItem RedPokemon where
  heldItem (Pikachu _ _) = LightBall
  heldItem (Lapras _ _) = SitrusBerry
  heldItem (Snorlax _ _) = QuickClaw
  heldItem (Venusaur _ _) = WhiteHerb
  heldItem (Charizard _ _) = FocusSash
  heldItem (Blastoise _ _) = ChoiceScarf

instance HeldItem BluePokemon where
  heldItem (Aerodactyl _ _) = ChoiceBand
  heldItem (Exeggutor _ _) = LifeOrb
  heldItem (Gyarados _ _) = KingsRock
  heldItem (Alakazam _ _) = FocusSash
  heldItem (Arcanine _ _) = ExpertBelt
  heldItem (Machamp _ _) = WhiteHerb

instance HeldItem LancePokemon where
  heldItem (LancePokemon pkmn _) = go pkmn
    where
      go (Dragonite _) = FocusSash
      go (Salamence _) = ExpertBelt
      go (Kingdra _) = ScopeLens
      go (Haxorus _) = ChoiceScarf
      go Hydreigon = WhiteHerb
      go Flygon = PowerHerb
 
instance HeldItem StevenPokemon where
  heldItem (Metagross _) = OccaBerry
  heldItem (Aggron _ _) = AirBalloon
  heldItem (Excadrill _ _) = FocusSash
  heldItem (Archeops _) = SitrusBerry
  heldItem (Cradily _ _) = ExpertBelt
  heldItem (Armaldo _ _) = WhiteHerb

instance HeldItem WallacePokemon where
  heldItem (WallacePartyMember p _) = go p 
    where
      go (Milotic _)  = RockyHelmet
      go (Sharpedo _) = FocusSash
      go (Walrein _)  = Leftovers
      go (Ludicolo _) = LifeOrb
  heldItem (Swampert _ _) = RindoBerry
  heldItem (Starmie _)    = ExpertBelt
