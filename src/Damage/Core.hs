module Damage.Core where

--import System.Random

--userAbility :: String -> UserAbility
--targetAbility :: String -> TargetAbility
--typeOf :: String -> Type
--userItem :: String -> IsConsumed -> UserItem
--targetItem :: String -> IsConsumed -> TargetItem
--weather :: String -> Weather
--moveName :: String -> MoveName
--withCriticalHit :: (IsCritical -> a) -> (Critical -> a)


data MoveName =
  OtherMove
  | Acrobatics
  | Assurance
  | Avalanche
  | Brine
  | Earthquake
  | FuryCutter
  | FusionBolt
  | FusionFlare
  | Gust
  | Hex
  | IceBall
  | HiddenPower
  | Payback
  | Present
  | Pursuit
  | Revenge
  | Rollout
  | SpitUp
  | Stomp
  | Surf
  | ThousandArrows
  | TrumpCard
  | Twister
  | Venoshock
  | WakeUpSlap
  | WeatherBall

type Modif = Int

data Type = 
  Normal 
  | Fighting
  | Flying
  | Ground
  | Psychic
  | Ghost
  | Dark
  deriving (Eq,Show,Ord,Enum,Bounded)

data Stat =
  Attack
  | Defense
  | SpAttack
  | SpDefense

data Counterparty = 
  User
  | Target

data TypeEffect =
  Ineffective
  | NotVeryEffective
  | Effective
  | SuperEffective
  deriving (Eq,Ord,Enum,Bounded)

typeEffectAsDouble :: TypeEffect -> Double
typeEffectAsDouble Ineffective = 0
typeEffectAsDouble NotVeryEffective = 0.5
typeEffectAsDouble Effective = 1
typeEffectAsDouble SuperEffective = 2

data DamageFormulaBuilder = DFB {
  moveCategory :: Category
  , moveName :: MoveName
  , moveBasepower :: Maybe Int
  --, seed :: StdGen
  , movePP :: Int
  , userUsedMoveInRow :: Int
  , userLevel :: Int
  , userHPIV, userAttackIV, userDefenseIV, 
  userSpAttackIV, userSpDefenseIV, userSpeedIV :: IsOdd
  , userStockpileCount :: Int
  , userFriendship :: Int
  , moveSoundBased, moveSelfDestructing :: Bool
  , basepowerDeterminant :: Maybe Int -> MoveName -> BasepowerCase
  , typeChartRule :: Type -> Type -> TypeEffect
}

data Category =
  Physical
  | Special

data Screen =
  Reflect
  | LightScreen

type Berry = Int -- TODO

data PaybackCase =
  TargetSwitchingOut 
  | TargetUsesItem
  | TargetMovesBeforeUser

type Percent = Double

data BasepowerCase =
  Basepower Int
  | UserTakenDamageThisTurn (Int -> Int)
  | TargetDamagedUserThisTurn (Int -> Int)
  | UserHP (Percent -> Int)
  | ItemWeight (Int -> Int)
  | TargetWeight (Double -> Int)
  | SpeedDifference (UserSpeed -> TargetSpeed -> Int)
  | BerryCapability (Int -> Int)
  | UserStatIncreaseAmount (Int -> Int)
  | TargetStatIncreaseAmount (Int -> Int)
  | WeightDifference (UserWeight -> TargetWeight -> Int)
  | UserHasItem (Bool -> Int)
  | TargetHP (Percent -> Int)
  | HPDifference (UserHP -> TargetHP -> Int)
  | AnyPokemonUsedFusionBolt (Bool -> Int)
  | AnyPokemonUsedFusionFlare (Bool -> Int)
  | TargetUnderground (Bool -> Int)
  | TargetUnderwater (Bool -> Int)
  | TargetMinimized (Bool -> Int)
  | TargetTriesSwitchingOut (Bool -> Int)
  | PaybackCase (PaybackCase -> Int)
  | WeatherOn (Bool -> Int)
  | TargetHasStatus (Status -> Int)
  | UserHasStatus (Status -> Int)

data Status =
  NoStatus
  | Burned
  | Frozen
  | Paralyzed
  | Poisoned
  | Sleep

type IsOdd = Bool

type UserWeight = Double
type TargetWeight = Double
type UserHP = Int
type TargetHP = Int
type UserSpeed = Int
type TargetSpeed = Int

-- Damage formula works under the assumption that following things are fixed when
-- the attacking move is declared:
-- -move category
-- 
-- In addtition, in a subset of Champions Tournament context, we make
-- the following assumptions for optimization purposes (if the target
-- performs switch, then use data on the switched Pokemon instead of the 
-- originally chosen target, unless Pursuit but none of the CT Pokemon
-- have that move):
-- -move type
-- -user type
-- -target type
-- -target weight
-- -user ability
-- -target ability
-- -user item
-- -no Miracle Eye or Foresight

data DamageFormulaF a =
  DF {
    userLevelComponent :: a
    , basepowerComponent :: a
    , nonCriticalHit :: a
    , criticalHit :: a
    , typeAdvantageComponent :: a
    , modifierComponent :: a
  }
  | Multiplier Double
  | ConsumedItem Counterparty
  | LVL {
    userLevelIs :: !Double
  }
  | BP BasepowerCase
  | Ratio {
    attackFrom :: !Counterparty
    , defenseFrom :: !Counterparty
    , attackStat :: !Stat
    , defenseStat :: !Stat
    , targetScreenOfInterest :: !Screen
    , targetHasScreen :: Int -> Int -> Double
    , targetHasNoScreen :: Int -> Int -> Double
  }
  | RatioCH {
     attackCFrom :: !Counterparty
    , defenseCFrom :: !Counterparty
    , attackCStat :: !Stat
    , defenseCStat :: !Stat
    , ratioCHResult :: (Int, Modif) -> (Int, Modif) -> Double
  }
  | TA {
    typeAdvantage :: (Type -> Type -> TypeEffect) -> Type -> [Type] -> Double
    , typeChart :: Type -> Type -> TypeEffect
    , superEffectiveCase :: a
    , notVeryEffectiveCase :: a
    , ineffectiveCase :: a
  }
  | IEC {
    targetHasRingTarget :: a
    , groundImmunityCase :: a
    , psychicImmunityCase :: a
    , normalImmunityCase :: a
    , otherImmunityCase :: Double
  }
  | GImmC {
    noGroundImmunityBypass :: Double
    , gravityInEffect :: a
    , targetGrounded :: a
  }
  | PImmC {
    noPsychicImmunityBypass :: Double
    , targetMiracleEyed :: a
  }
  | NImmC {
    noNormalImmunityBypass :: Double
    , targetForesighted :: a
    , targetOdorSleuthed :: a
  }
  | SEC {
    userHasExpertBelt :: a
    , targetHasFilter, targetHasPrismArmor, targetHasSolidRock :: a
    , targetHasSEBerry :: [((Type, Berry), a)]
  }

data Seed = 
  Leaf Double
  | TopLevel
  | LvlPhase
  | BPPhase
  | NonCritPhase
  | CritPhase
  | TypeAdvPhase ((Type -> Type -> TypeEffect) -> (Type -> Type -> TypeEffect))
  | SuperEffPhase
  | NotVeryEffPhase
  | IneffPhase
  | GrndImmPhase
  | PscImmPhase
  | NrmlImmPhase
  | ModifPhase

formulaCoalg :: DamageFormulaBuilder -> Seed -> DamageFormulaF Seed
formulaCoalg _ (Leaf n) = Multiplier n
formulaCoalg _ TopLevel = DF {
    userLevelComponent = LvlPhase
    , basepowerComponent = BPPhase
    , nonCriticalHit = NonCritPhase
    , criticalHit = CritPhase
    , typeAdvantageComponent = TypeAdvPhase id -- Moves with modified type effect implement later TODO
    , modifierComponent = ModifPhase
  }
formulaCoalg b LvlPhase = LVL $ (2 * fromIntegral (userLevel b))/5 + 2
formulaCoalg b BPPhase = BP $ case moveBasepower b of
  Just n -> Basepower n
  Nothing -> basepowerDeterminant b Nothing . moveName $ b
formulaCoalg b NonCritPhase = case moveCategory b of
  Physical -> Ratio {
    attackFrom = User
    , defenseFrom = Target
    , attackStat = Attack
    , defenseStat = Defense
    , targetScreenOfInterest = Reflect
    , targetHasScreen = \usrStat tgtStat -> fromIntegral usrStat / (2 * fromIntegral tgtStat)
    , targetHasNoScreen = \usrStat tgtStat -> fromIntegral usrStat / fromIntegral tgtStat
  }
  Special -> Ratio {
    attackFrom = User
    , defenseFrom = Target
    , attackStat = SpAttack
    , defenseStat = SpDefense
    , targetScreenOfInterest = LightScreen
    , targetHasScreen = \usrStat tgtStat -> fromIntegral usrStat / (2 * fromIntegral tgtStat)
    , targetHasNoScreen = \usrStat tgtStat -> fromIntegral usrStat / fromIntegral tgtStat
  }
formulaCoalg b (TypeAdvPhase f) = TA {
    typeAdvantage = \tc mvType tgtType -> foldr (*) 1 $ fmap (typeEffectAsDouble . (f tc) mvType) $ tgtType 
    , typeChart = typeChartRule b
    , superEffectiveCase = SuperEffPhase
    , notVeryEffectiveCase = NotVeryEffPhase
    , ineffectiveCase = IneffPhase
  }
formulaCoalg _ SuperEffPhase = SEC {
    userHasExpertBelt = Leaf 1.2
    , targetHasSolidRock = Leaf 0.75
    , targetHasFilter = Leaf 0.75
    , targetHasPrismArmor = Leaf 0.75
  }
formulaCoalg _ IneffPhase = IEC {
    targetHasRingTarget = TypeAdvPhase (\f mvt tt -> case f mvt tt of
      Ineffective -> Effective
      a -> a) 
    , groundImmunityCase = GrndImmPhase
    , psychicImmunityCase = PscImmPhase
    , normalImmunityCase = NrmlImmPhase
    , otherImmunityCase = 0
  }
formulaCoalg _ GrndImmPhase = GImmC {
    noGroundImmunityBypass = 0
    , gravityInEffect = TypeAdvPhase g
    , targetGrounded = TypeAdvPhase g
  }
  where
    g f Ground t = if f Ground t == Ineffective then Effective else f Ground t 
    g f m t      = f m t
formulaCoalg _ PscImmPhase = PImmC {
    noPsychicImmunityBypass = 0
    , targetMiracleEyed = TypeAdvPhase (\f mvt tt -> if mvt == Psychic && f mvt tt == Ineffective 
      then Effective
      else f mvt tt)
  }
formulaCoalg _ NrmlImmPhase = NImmC {
    noNormalImmunityBypass = 0
    , targetForesighted = TypeAdvPhase g
    , targetOdorSleuthed = TypeAdvPhase g
  }
  where
    g f Normal t = if f Normal t == Ineffective then Effective else f Normal t
    g f m t      = f m t




