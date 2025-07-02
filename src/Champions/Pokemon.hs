module Champions.Pokemon where

import qualified Party.Gender as Gender
import Champions.Ability
import System.Random
import Data.List (reverse)
import qualified Party.Gender as Gender

data RedPokemon =
  Pikachu PikachuAbility PikachuGender
  | Lapras LaprasAbility LaprasGender 
  | Snorlax SnorlaxAbility SnorlaxGender
  | Venusaur VenusaurAbility VenusaurGender
  | Charizard CharizardAbility CharizardGender
  | Blastoise BlastoiseAbility BlastoiseGender
  deriving (Eq,Ord,Show)

type PikachuGender = Gender.Gender Bool
type LaprasGender = Gender.Gender Bool
type SnorlaxGender = Gender.Gender Gender.SevenOutOfEight
type VenusaurGender = Gender.Gender Gender.SevenOutOfEight
type CharizardGender = Gender.Gender Gender.SevenOutOfEight
type BlastoiseGender = Gender.Gender Gender.SevenOutOfEight


data PikachuAbility =
 PikachuStatic
 | PikachuElectricRod
  deriving (Eq,Ord,Bounded,Enum)

instance Show PikachuAbility where
  show PikachuStatic = "Static"
  show PikachuElectricRod = "ElectricRod"

randomAbilityR :: (Enum ab, RandomGen g) => (ab,ab) -> g -> (ab, g)
randomAbilityR (a,b) g = let 
  (r,g') = randomR (fromEnum a, fromEnum b) g
    in (toEnum r, g')
      
randomAbility :: (Enum ab, RandomGen g) => g -> (ab, g) 
randomAbility = randomAbilityR (toEnum 0, head . reverse $ [toEnum 0..])

instance Random PikachuAbility where
  randomR = randomAbilityR 
  random  = randomAbility
instance Random LaprasAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random SnorlaxAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random VenusaurAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random CharizardAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random BlastoiseAbility where
  randomR = randomAbilityR
  random  = randomAbility
 

data LaprasAbility =
 LaprasWaterAbsorb
 | LaprasShellArmor
 | LaprasHydration
  deriving (Eq,Ord,Bounded,Enum)

instance Show LaprasAbility where
  show LaprasWaterAbsorb = "WaterAbsorb"
  show LaprasShellArmor = "ShellArmor"
  show LaprasHydration = "Hydration"


data SnorlaxAbility = 
 SnorlaxImmunity
 | SnorlaxThickFat
 | SnorlaxGluttony
  deriving (Eq,Ord,Bounded,Enum)

instance Show SnorlaxAbility where
  show SnorlaxImmunity = "Immunity"
  show SnorlaxThickFat = "ThickFat"
  show SnorlaxGluttony = "Gluttony"


data VenusaurAbility =
 VenusaurOvergrow
 | VenusaurChlorophyll
  deriving (Eq,Ord,Bounded,Enum)

instance Show VenusaurAbility where
  show VenusaurOvergrow = "Overgrow"
  show VenusaurChlorophyll = "Chlorophyll"


data CharizardAbility =
 CharizardBlaze
 | CharizardSolarPower
  deriving (Eq,Ord,Bounded,Enum)

instance Show CharizardAbility where
  show CharizardBlaze = "Blaze"
  show CharizardSolarPower = "SolarPower"


data BlastoiseAbility =
 BlastoiseTorrent
 | BlastoiseRainDish
  deriving (Eq,Ord,Bounded,Enum)

instance Show BlastoiseAbility where
  show BlastoiseTorrent = "Torrent"
  show BlastoiseRainDish = "RainDish"


instance StaticAbility PikachuAbility where
  static = PikachuStatic
instance ElectricRodAbility PikachuAbility where
  electricRod = PikachuElectricRod
instance WaterAbsorbAbility LaprasAbility where
  waterAbsorb = LaprasWaterAbsorb
instance ShellArmorAbility LaprasAbility where
  shellArmor = LaprasShellArmor
instance HydrationAbility LaprasAbility where
  hydration = LaprasHydration
instance ImmunityAbility SnorlaxAbility where
  immunity = SnorlaxImmunity
instance ThickFatAbility SnorlaxAbility where
  thickFat = SnorlaxThickFat
instance GluttonyAbility SnorlaxAbility where
  gluttony = SnorlaxGluttony
instance OvergrowAbility VenusaurAbility where
  overgrow = VenusaurOvergrow
instance ChlorophyllAbility VenusaurAbility where
  chlorophyll = VenusaurChlorophyll
instance BlazeAbility CharizardAbility where
  blaze = CharizardBlaze
instance SolarPowerAbility CharizardAbility where
  solarPower = CharizardSolarPower
instance TorrentAbility BlastoiseAbility where
  torrent = BlastoiseTorrent
instance RainDishAbility BlastoiseAbility where
  rainDish = BlastoiseRainDish
 

data BluePokemon =
  Aerodactyl AerodactylAbility AerodactylGender 
  | Exeggutor ExeggutorAbility ExeggutorGender 
  | Gyarados GyaradosAbility GyaradosGender 
  | Alakazam AlakazamAbility AlakazamGender 
  | Arcanine ArcanineAbility ArcanineGender 
  | Machamp MachampAbility MachampGender 
  deriving (Eq,Show,Ord)

type AerodactylGender = Gender.Gender Gender.SevenOutOfEight 
type ExeggutorGender  = Gender.Gender Bool
type GyaradosGender   = Gender.Gender Bool
type AlakazamGender   = Gender.Gender Gender.ThreeOutOfFour
type ArcanineGender   = Gender.Gender Gender.ThreeOutOfFour
type MachampGender    = Gender.Gender Gender.ThreeOutOfFour 


instance Random AerodactylAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random ExeggutorAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random GyaradosAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random AlakazamAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random ArcanineAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random MachampAbility where
  randomR = randomAbilityR
  random  = randomAbility
 


data AerodactylAbility = 
  AerodactylRockHead
  | AerodactylPressure
  | AerodactylUnnerve
  deriving (Eq,Ord,Enum,Bounded)

instance Show AerodactylAbility where
  show AerodactylRockHead = "RockHead"
  show AerodactylPressure = "Pressure"
  show AerodactylUnnerve = "Unnerve"


data ExeggutorAbility = 
  ExeggutorChlorophyll
  | ExeggutorHarvest
  deriving (Eq,Ord,Enum,Bounded)

instance Show ExeggutorAbility where
  show ExeggutorChlorophyll = "Chlorophyll"
  show ExeggutorHarvest = "Harvest"


data GyaradosAbility = 
  GyaradosIntimidate
  | GyaradosMoxie
  deriving (Eq,Ord,Enum,Bounded)

instance Show GyaradosAbility where
  show GyaradosIntimidate = "Intimidate"
  show GyaradosMoxie = "Moxie"


data AlakazamAbility = 
  AlakazamSynchronize
  | AlakazamInnerFocus
  | AlakazamMagicGuard
  deriving (Eq,Ord,Enum,Bounded)

instance Show AlakazamAbility where
  show AlakazamSynchronize = "Synchronize"
  show AlakazamInnerFocus = "InnerFocus"
  show AlakazamMagicGuard = "MagicGuard"


data ArcanineAbility = 
  ArcanineIntimidate
  | ArcanineFlashFire
  | ArcanineJustified
  deriving (Eq,Ord,Enum,Bounded)

instance Show ArcanineAbility where
  show ArcanineIntimidate = "Intimidate"
  show ArcanineFlashFire = "FlashFire"
  show ArcanineJustified = "Justified"


data MachampAbility = 
  MachampGuts
  | MachampNoGuard
  | MachampSteadfast
  deriving (Eq,Ord,Enum,Bounded)

instance Show MachampAbility where
  show MachampGuts = "Guts"
  show MachampNoGuard = "NoGuard"
  show MachampSteadfast = "Steadfast"

instance RockHeadAbility AerodactylAbility where
  rockHead = AerodactylRockHead
instance PressureAbility AerodactylAbility where
  pressure = AerodactylPressure
instance UnnerveAbility AerodactylAbility where
  unnerve = AerodactylUnnerve
instance ChlorophyllAbility ExeggutorAbility where
  chlorophyll = ExeggutorChlorophyll
instance HarvestAbility ExeggutorAbility where
  harvest = ExeggutorHarvest
instance IntimidateAbility GyaradosAbility where
  intimidate = GyaradosIntimidate
instance MoxieAbility GyaradosAbility where
  moxie = GyaradosMoxie
instance SynchronizeAbility AlakazamAbility where
  synchronize = AlakazamSynchronize
instance InnerFocusAbility AlakazamAbility where
  innerFocus = AlakazamInnerFocus
instance MagicGuardAbility AlakazamAbility where
  magicGuard = AlakazamMagicGuard
instance IntimidateAbility ArcanineAbility where
  intimidate = ArcanineIntimidate
instance FlashFireAbility ArcanineAbility  where
  flashFire = ArcanineFlashFire
instance JustifiedAbility ArcanineAbility where
  justified = ArcanineJustified
instance GutsAbility MachampAbility where
  guts = MachampGuts
instance NoGuardAbility MachampAbility where
  noGuard = MachampNoGuard
instance SteadfastAbility MachampAbility where
  steadfast = MachampSteadfast

data LancePokemon = LancePokemon LancePartyMember (Gender.Gender Bool) deriving (Eq)

instance Show LancePokemon where
  show (LancePokemon p g) = show p <> " " <> show g

data LancePartyMember =
  Dragonite DragoniteAbility
  | Salamence SalamenceAbility
  | Kingdra KingdraAbility
  | Haxorus HaxorusAbility
  | Hydreigon
  | Flygon
  deriving (Eq,Show,Ord)

instance Random DragoniteAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random KingdraAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random HaxorusAbility where
  randomR = randomAbilityR
  random  = randomAbility

type SalamenceAbility = GyaradosAbility

data DragoniteAbility =
 DragoniteInnerFocus
 | DragoniteMultiscale
 deriving (Eq,Ord,Enum,Bounded)


instance Show DragoniteAbility where
 show DragoniteInnerFocus = "InnerFocus"
 show DragoniteMultiscale = "Multiscale"


data KingdraAbility =
 KingdraSwiftSwim
 | KingdraSniper
 | KingdraDamp
 deriving (Eq,Ord,Enum,Bounded)

instance Show KingdraAbility where
 show KingdraSwiftSwim = "SwiftSwim"
 show KingdraSniper = "Sniper"
 show KingdraDamp = "Damp"
 

data HaxorusAbility =
 HaxorusRivalry
 | HaxorusMoldBreaker
 | HaxorusUnnerve
 deriving (Eq,Ord,Enum,Bounded)

instance Show HaxorusAbility where
 show HaxorusRivalry = "Rivalry"
 show HaxorusMoldBreaker = "MoldBreaker"
 show HaxorusUnnerve = "Unnerve"


instance InnerFocusAbility DragoniteAbility where
  innerFocus = DragoniteInnerFocus
instance MultiscaleAbility DragoniteAbility where
  multiscale = DragoniteMultiscale
instance SwiftSwimAbility KingdraAbility where
  swiftSwim = KingdraSwiftSwim
instance SniperAbility KingdraAbility where
  sniper = KingdraSniper
instance DampAbility KingdraAbility where
  damp = KingdraDamp
instance RivalryAbility HaxorusAbility where
  rivalry = HaxorusRivalry
instance MoldBreakerAbility HaxorusAbility where
  moldBreaker = HaxorusMoldBreaker
instance UnnerveAbility HaxorusAbility where
  unnerve = HaxorusUnnerve

data StevenPokemon =
  Metagross MetagrossAbility
  | Aggron AggronAbility AggronGender
  | Excadrill ExcadrillAbility ExcadrillGender
  | Archeops ArcheopsGender
  | Cradily CradilyAbility CradilyGender
  | Armaldo ArmaldoAbility ArmaldoGender
  deriving (Eq,Ord)

type CradilyGender = Gender.Gender Gender.SevenOutOfEight
type ArmaldoGender = Gender.Gender Gender.SevenOutOfEight
type AggronGender = Gender.Gender Bool
type ExcadrillGender = Gender.Gender Bool
type ArcheopsGender = Gender.Gender Gender.SevenOutOfEight

instance Random MetagrossAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random AggronAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random ExcadrillAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random CradilyAbility where
  randomR = randomAbilityR
  random  = randomAbility
instance Random ArmaldoAbility where
  randomR = randomAbilityR
  random  = randomAbility
 

data MetagrossAbility =
  MetagrossClearBody
  | MetagrossLightMetal
  deriving (Eq,Ord,Enum,Bounded)

instance Show MetagrossAbility where
  show MetagrossClearBody = "ClearBody"
  show MetagrossLightMetal = "LightMetal"

instance ClearBodyAbility MetagrossAbility where
  clearBody = MetagrossClearBody 
instance LightMetalAbility MetagrossAbility where
  lightMetal = MetagrossLightMetal 

data AggronAbility =
  AggronSturdy
  | AggronRockHead
  | AggronHeavyMetal
  deriving (Eq,Ord,Enum,Bounded)

instance Show AggronAbility where
  show AggronSturdy = "Sturdy"
  show AggronRockHead = "RockHead"
  show AggronHeavyMetal = "HeavyMetal"

instance SturdyAbility AggronAbility where
  sturdy = AggronSturdy 
instance RockHeadAbility AggronAbility where
  rockHead = AggronRockHead 
instance HeavyMetalAbility AggronAbility where
  heavyMetal = AggronHeavyMetal 

data ExcadrillAbility =
  ExcadrillSandRush
  | ExcadrillSandForce
  deriving (Eq,Ord,Enum,Bounded)

instance Show ExcadrillAbility where
  show ExcadrillSandRush = "SandRush"
  show ExcadrillSandForce = "SandForce"

instance SandRushAbility ExcadrillAbility where
  sandRush = ExcadrillSandRush 
instance SandForceAbility ExcadrillAbility where
  sandForce = ExcadrillSandForce 

data CradilyAbility =
  CradilySuctionCups
  | CradilyStormDrain
  deriving (Eq,Ord,Enum,Bounded)

instance Show CradilyAbility where
  show CradilySuctionCups = "SuctionCups"
  show CradilyStormDrain = "StormDrain"

instance SuctionCupsAbility CradilyAbility where
  suctionCups = CradilySuctionCups 
instance StormDrainAbility CradilyAbility where
  stormDrain = CradilyStormDrain 

data ArmaldoAbility = 
  ArmaldoBattleArmor
  | ArmaldoSwiftSwim
  deriving (Eq,Ord,Enum,Bounded)

instance Show ArmaldoAbility where 
  show ArmaldoBattleArmor = "BattleArmor"
  show ArmaldoSwiftSwim = "SwiftSwim"

instance BattleArmorAbility ArmaldoAbility where
  battleArmor = ArmaldoBattleArmor 
instance SwiftSwimAbility ArmaldoAbility where
  swiftSwim = ArmaldoSwiftSwim 

data WallacePokemon =
  WallacePartyMember WallacePartyMember (Gender.Gender Bool)
  | Swampert SwampertAbility SwampertGender
  | Starmie StarmieAbility 
  deriving (Eq,Ord,Show)

type SwampertGender = Gender.Gender Gender.SevenOutOfEight

data WallacePartyMember =  
  Milotic MiloticAbility 
  | Sharpedo SharpedoAbility 
  | Walrein WalreinAbility 
  | Ludicolo LudicoloAbility
  deriving (Eq,Ord,Show)

data MiloticAbility =
  MiloticMarvelScale
  | MiloticCompetitive
  | MiloticCuteCharm 
  deriving (Eq,Ord,Enum,Bounded)
data SharpedoAbility = 
  SharpedoRoughSkin
  | SharpedoSpeedBoost
  deriving (Eq,Ord,Enum,Bounded)
data WalreinAbility = 
  WalreinThickFat
  | WalreinIceBody
  | WalreinOblivious
  deriving (Eq,Ord,Enum,Bounded)
data LudicoloAbility =
  LudicoloSwiftSwim
  | LudicoloRainDish
  | LudicoloOwnTempo
  deriving (Eq,Ord,Enum,Bounded)
data SwampertAbility = 
  SwampertTorrent
  | SwampertDamp
  deriving (Eq,Ord,Enum,Bounded)
data StarmieAbility =
  StarmieIlluminate
  | StarmieNaturalCure
  | StarmieAnalytic
  deriving (Eq,Ord,Enum,Bounded)

instance Show MiloticAbility where
  show MiloticMarvelScale = "MarvelScale"
  show MiloticCompetitive = "Competitive"
  show MiloticCuteCharm  = "CuteCharm "
instance Show SharpedoAbility where 
  show SharpedoRoughSkin = "RoughSkin"
  show SharpedoSpeedBoost = "SpeedBoost"
instance Show WalreinAbility where 
  show WalreinThickFat = "ThickFat"
  show WalreinIceBody = "IceBody"
  show WalreinOblivious = "Oblivious"
instance Show LudicoloAbility where
  show LudicoloSwiftSwim = "SwiftSwim"
  show LudicoloRainDish = "RainDish"
  show LudicoloOwnTempo = "OwnTempo"
instance Show SwampertAbility where 
  show SwampertTorrent = "Torrent"
  show SwampertDamp = "Damp"
instance Show StarmieAbility where
  show StarmieIlluminate = "Illuminate"
  show StarmieNaturalCure = "NaturalCure"
  show StarmieAnalytic = "Analytic"

--instance MarvelScaleAbility MiloticAbility where
--  marvelScale = MiloticMarvelScale 
--  MiloticCompetitive
--  MiloticCuteCharm 
--  deriving (Eq,Ord,Enum,Bounded)
--data SharpedoAbility = 
--  SharpedoRoughSkin
--  SharpedoSpeedBoost
--  deriving (Eq,Ord,Enum,Bounded)
--data WalreinAbility = 
--  WalreinThickFat
--  WalreinIceBody
--  WalreinOblivious
--  deriving (Eq,Ord,Enum,Bounded)
--data LudicoloAbility =
--  LudicoloSwiftSwim
--  LudicoloRainDish
--  LudicoloOwnTempo
--  deriving (Eq,Ord,Enum,Bounded)
--data SwampertAbility = 
--  SwampertTorrent
--  SwampertDamp
--  deriving (Eq,Ord,Enum,Bounded)
--data StarmieAbility =
--  StarmieIlluminate
--  StarmieNaturalCure
--  StarmieAnalytic
--  deriving (Eq,Ord,Enum,Bounded)




--MiloticAbility 
--SharpedoAbility 
--WalreinAbility 
--LudicoloAbility
--SwampertAbility 
--StarmieAbility

