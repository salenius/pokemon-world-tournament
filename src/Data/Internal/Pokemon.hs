{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Data.Internal.Pokemon (
  Pokemon()
  -- Stats
  ,hp,attack,defence,specialAttack,specialDefence,speed

  -- Constructors
  ,fromJson
  ,ditto

  -- Functor, bifunctor, applicative and monad API tools
  ,mapSpec
  ,mapIndv
  ,transform
  ,getSpec
  ,getIndv
  ,pullSpec
  ,pullIndv

  -- Getters
  ,name
  ,type1
  ,type2
  ,base
  ,weight
  ,height
  ,baseExp
  ,catchRate
  ,isLegendary
  ,isMythical
  ,hasPreviousEvolutions
  ,evolvesFurther

  -- Setters
  ,sName
  ,sType1
  ,sType2
  ,sBase
  ,sWeight
  ,sHeight
  ,sBaseExp
  ,sCatchRate
  ,sIsLegendary
  ,sIsMythical
  ,sHasPreviousEvolutions
  ,sEvolvesFurther
 
  
                             ) where

{-

Copyright (C) Tommi Salenius, 2025

This module contains the bare implementation
of the Pokemon creature in the game. The Pokemon
are created from JSON strings.

In this model, Pokemon is a bifunctor Pokemon s i
where `s` specifies the species-specific attributes
of the Pokemon, and `i` specifies the individual-specific
attributes of it. Using these names for the type abstractions
conveys the meaning, that they are meant to be extended
by the library writer.

`a` and `b` are type abstractions that are meant for
the library users. 

As naming convention, getters have the name of the attribute
(eg. 'name'), and setters start with 's', followed by the
attribute name capitalized (eg. 'sName'). Only exception to
this rule are 'spec' (= species) and 'indv' (= individual)
where getters are 'pullSpec' and 'pullIndv', and setters
are 'mapSpec' and 'mapIndv' respectively. The reason here
is that eg. mapSpec does not merely set a new attribute but
it possibly transforms the attribute type into another.

-}

import Data.Bifunctor
import qualified Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Units
import Data.Stats
import Data.List (groupBy,sortBy)
import Data.Text hiding (filter,elem,head,length,foldr,maximum,empty,groupBy,map)
import Data.Char hiding (isAscii)
import Data.Internal.Type
import Data.Maybe (fromMaybe,listToMaybe)
import Data.ByteString.Lazy hiding (filter,elem,head,length,foldr,maximum,empty,groupBy,map)
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad
import Data.Scientific (toRealFloat,Scientific(..))

data Pokemon s i = PokemonPrim {
  _species :: s
  , _individual :: i
  , _name :: !Text
  , _statCalc :: Stat -> Int
  , _type :: (Type, Maybe Type)
  , _weight :: Kg
  , _height :: Meters
  , _catchR, _bExp :: Int
  , _specInfo :: [SpecInfo]
  }

data SpecInfo = Legendary | Mythical | NextEvolutions | PreviousEvolutions deriving (Eq,Enum,Bounded)

instance Show (Pokemon s i) where
  show p = show $ name p

-- | Creates a blank Pokemon species from a JSON. The JSON must be
-- an object, which is created by the pokemon-data.sh shell script
-- located at the projects `data` folder (the script creates an
-- array of JSON objects, so the input here is the element of that array).
-- The other argument is a function, which takes a JSON string an produces
-- species specific data object of it, which contains data that can be
-- accessed and manipulated by `pullSpec`, `mapSpec` and `spec` functions.
fromJson :: (ByteString -> Maybe s) -> ByteString -> Maybe (Pokemon s ())
fromJson st txt = do
  pkmn <- Aeson.decode txt :: Maybe (Pokemon () ())
  spec <- st txt
  return $ bimap (const spec) (const ()) pkmn

-- | Represents the Pokemon Ditto, semantically equivalent to one that is
-- produced by the `fromJson` function, where the input is created from
-- the command line by: jq '.[] | select(.name == "ditto")' pokemon.json
ditto :: s -> i -> Pokemon s i
ditto s i = PokemonPrim {
  _species = s
  , _individual = i
  , _name = "ditto"
  , _statCalc = const 48
  , _type = (Normal, Nothing)
  , _weight = Kg 4
  , _height = Meters 0.3
  , _catchR = 35
  , _bExp = 101
  , _specInfo = []
  }

-- | Maps species information from one type to another, satisfies functor laws.
mapSpec :: (s -> t) -> Pokemon s i -> Pokemon t i
mapSpec f p = PokemonPrim {
  _species = f (getSpec p)
  , _individual = getIndv p
  , _name = name p
  , _statCalc = flip base p
  , _type = (type1 p, type2 p)
  , _weight = weight p
  , _height = height p
  , _catchR = catchRate p
  , _bExp = baseExp p
  , _specInfo = _specInfo p
  }

-- | Maps individual information from one type to another, satisfies functor laws.
mapIndv :: (i -> j) -> Pokemon s i -> Pokemon s j
mapIndv f p = PokemonPrim {
  _species = getSpec p
  , _individual = f $ getIndv p
  , _name = name p
  , _statCalc = flip base p
  , _type = (type1 p, type2 p)
  , _weight = weight p
  , _height = height p
  , _catchR = catchRate p
  , _bExp = baseExp p
  , _specInfo = _specInfo p
  }



-- | Satisfies the following laws:
-- pullSpec p x == mapSpec ($ x) p
pullSpec :: Pokemon (x -> s) i -> x -> Pokemon s i
pullSpec p x = mapSpec ($ x) p

-- | Satisfies the following laws:
-- pullIndv p x == mapIndv ($ x) p
pullIndv :: Pokemon s (x -> i) -> x -> Pokemon s i
pullIndv p x = mapIndv ($ x) p

-- | Flattens Pokemon by taking the inner Pokemon from individual slot,
-- and replaces all the values of the outer Pokemon by its own data
--
-- Properties satisfied:
-- ^^^^^^^^^
-- transform (ditto s i) == i
-- trasform p == getIndv p
-- trasform (mapIndv (f . const q) p) == f q
transform :: Pokemon s (Pokemon s i) -> Pokemon s i
transform = getIndv

getSpec :: Pokemon s i -> s
getSpec = _species

getIndv :: Pokemon s i -> i
getIndv = _individual

-- | Get the Pokemon name. Name is always a non-empty string
-- and it must begin with an ASCII alphabet character.
-- It also may contain dash or numbers; numbers can be
-- either in the middle or in the end, dash must always
-- be in the middle if they exist. Also the names
-- have upper limit of 256 characters.
name :: Pokemon s i -> Text
name = _name
 
-- | Sets Pokemon name according to the function given
-- as the first argument. The function must transform
-- the name according to rules listed under the `name` function,
-- otherwise the original Pokemon is returned as a result.
sName :: (Text -> Text) -> Pokemon s i -> Pokemon s i
sName t p
  | not . validPName . name $ p = p
  | otherwise = p {_name = t . name $ p}

-- | Calculates whether the Pokemon name is valid. Should
-- be transferred to a different module later on.
validPName :: Text -> Bool
validPName s =
  Text.length s > 0
  && Text.length s < 256
  && isAscii s && fvalid && lvalid && rstvld
  where
    fvalid = isAlpha . Text.head $ s
    lvalid = isAlphaNum . Text.last $ s
    rstvld = Text.all andash
          . Text.tail
          . Text.reverse
          . Text.tail
          $ s
    andash = (||) <$> isAlphaNum <*> (==) '-'

-- | Returns the Pokemon's primary type, where the type is one of the fifteen
-- basic types from Generation I. If the library user wants to use
-- later generation defined types (Steel, Dark, Fairy) or maybe their
-- own custom types, they should implement a wrapper function and store
-- the information about the type in to the field that `getSpec` function
-- is able to access.
type1 :: Pokemon s i -> Type
type1 = fst . _type

sType1 :: (Type -> Type) -> Pokemon s i -> Pokemon s i
sType1 f p = p {_type = (f (type1 p), type2 p)}

-- | May return the Pokemon's secondary type, where the type is one of the fifteen
-- basic types from Generation I. If the library user wants to use
-- later generation defined types (Steel, Dark, Fairy) or maybe their
-- own custom types, they should implement a wrapper function and store
-- the information about the type in to the field that `getSpec` function
-- is able to access.
type2 :: Pokemon s i -> Maybe Type
type2 = snd . _type

sType2 :: (Maybe Type -> Maybe Type) -> Pokemon s i -> Pokemon s i
sType2 f p = p {_type = (type1 p, f (type2 p))}

hp, attack, defence, speed, specialAttack, specialDefence :: Stat
hp = HP
attack = Attack
defence = Defence
speed = Speed
specialAttack = SpAttack
specialDefence = SpDefence

-- | Given the stat, calculates the species specific base stat for
-- given Pokemon. The stat values are guaranteed to be strictly
-- positive integers.
-- If the user wants to emulate Generation I where instead of separate
-- Special Attack and Special Defence there was only Special statistic,
-- they can create a wrapper function around this so that an argument
-- `special` will return either this function's result for Special Attack
-- or Special Defence (also for consistency, these two stats should be equal)
base :: Stat -> Pokemon s i -> Int
base st pk = _statCalc pk st

sBase :: (Int -> Int) -> Stat -> Pokemon s i -> Pokemon s i
sBase f st pk = pk {_statCalc =
                    \s -> if (s == st) && (f (_statCalc pk s) > 0)
                    then f (_statCalc pk s)
                    else _statCalc pk s}

-- | Returns the Pokemon's weight in kilograms. The attribute
-- is guaranteed to be a strictly positive real number that
-- is less than 1000.
weight :: Pokemon s i -> Kg
weight = _weight

sWeight :: (Kg -> Maybe Kg) -> Pokemon s i -> Pokemon s i
sWeight f pk = pk {_weight = fromMaybe (weight pk) (f . weight $ pk)}

-- | Returns the Pokemon's height in meters. The attribute
-- is guaranteed to be a strictly positive real number
-- that is less than 100.
height :: Pokemon s i -> Meters
height = _height

sHeight :: (Meters -> Maybe Meters) -> Pokemon s i -> Pokemon s i
sHeight f pk = pk {_height = fromMaybe (height pk) (f . height $ pk)}

-- | Returns the species specific numerical constant that determines
-- how easy it is to catch the Pokemon in the wild. The number is
-- integer between 0 and 255 (limits included).
catchRate :: Pokemon s i -> Int
catchRate = _catchR

sCatchRate :: (Int -> Int) -> Pokemon s i -> Pokemon s i
sCatchRate f pk = pk {_catchR = if result > 255 || result < 0 then _catchR pk else result}
  where
    result = f . _catchR $ pk

-- | Base experience that Pokemon gains at least minimum when they defeat an opponent
-- in the battle. This function guarantees that the return value is positive, no other
-- restrictions have been imposed otherwise.
baseExp :: Pokemon s i -> Int
baseExp = _bExp

sBaseExp :: (Int -> Int) -> Pokemon s i -> Pokemon s i
sBaseExp f pk = pk {_bExp = if result < 0 then _bExp pk else result}
  where
    result = f . _bExp $ pk

-- | If the Pokemon is question is legendary and also possibly mythical.
-- All mythical Pokemon are legendary, but not all legendary Pokemon are
-- mythical.
isLegendary, isMythical :: Pokemon s i -> Bool
isLegendary pk = Legendary `elem` _specInfo pk
isMythical pk = Mythical `elem` _specInfo pk

-- | Sets up the legendarity and mythicality status attribute for a given
-- Pokemon while respecting the domain rules that `isLegendary` and
-- `isMythical` functions obey.
sIsLegendary, sIsMythical :: (Bool -> Bool) -> Pokemon s i -> Pokemon s i
sIsLegendary f pk = case f (Legendary `elem` _specInfo pk) of
  True  -> pk {_specInfo = Legendary : (_specInfo pk)}
  False -> pk {_specInfo = filter (\s -> s /= Legendary || s /= Mythical) (_specInfo pk)}
sIsMythical f pk = case f (Mythical `elem` _specInfo pk) of
  True  -> pk {_specInfo = Legendary : Mythical : (_specInfo pk)}
  False -> pk {_specInfo = filter ((/=) Mythical) (_specInfo pk)}

hasPreviousEvolutions, evolvesFurther :: Pokemon s i -> Bool
hasPreviousEvolutions pk = PreviousEvolutions `elem` _specInfo pk
evolvesFurther pk = NextEvolutions `elem` _specInfo pk
 
sHasPreviousEvolutions, sEvolvesFurther :: (Bool -> Bool) -> Pokemon s i -> Pokemon s i
sHasPreviousEvolutions f pk = case f (PreviousEvolutions `elem` _specInfo pk) of
  True -> pk {_specInfo = PreviousEvolutions : (_specInfo pk)}
  False -> pk {_specInfo = filter ((/=) PreviousEvolutions) (_specInfo pk)}
sEvolvesFurther f pk = case f (NextEvolutions `elem` _specInfo pk) of
  True -> pk {_specInfo = NextEvolutions : (_specInfo pk)}
  False -> pk {_specInfo = filter ((/=) NextEvolutions) (_specInfo pk)}


instance Functor (Pokemon s) where
  fmap = mapIndv

instance Bifunctor Pokemon where
  bimap f g = mapSpec f . mapIndv g

instance Monoid s => Applicative (Pokemon s) where
  pure = ditto mempty
  f <*> x = fmap (getIndv f) x

instance Monoid s => Monad (Pokemon s) where
  return = pure
  m >>= f = transform (fmap f m)

specCondToList :: (a -> Bool) -> [SpecInfo] -> a -> [SpecInfo]
specCondToList cond spc x = if cond x then spc else []

instance (Aeson.FromJSON a, Aeson.FromJSON b) => Aeson.FromJSON (Pokemon a b) where
  parseJSON ob = withObject "Pokemon" (\v -> do

    s <- parseJSON ob
    i <- parseJSON ob
    
    name' <- v .: "name"

    Stats (
      baseHp
      ,baseAttack
      ,baseDefense
      ,baseSattack
      ,baseSdefense
      ,baseSpeed
          ) <- parseJSON ob
    
    weight' <- kg <$> parsePhysio v "weight"
    height' <- meters <$> parsePhysio v "height"
    
    species <- v .: "species"
    captureRate <- parsePositiveInt "capture_rate" species

    baseExperience <- parsePositiveInt "base_experience" v

    let prevEvolutions = [] -- Not yet possíble to access data from JSON
    isLegendary' <- specCondToList id [Legendary] <$> species .: "is_legendary"
    -- By default, in the Veekun data set, Pokemon are not classified as legendary
    -- if they are already mythical. We are doing a different classification here
    -- such that all mythical Pokemon are also legendary, but not vice-versa.
    isMythical' <- specCondToList id [Legendary,Mythical] <$> species .: "is_mythical"
    nextEvolutions <- specCondToList ((<) 0 . length) [NextEvolutions] <$> do
      Aeson.Array ar <- species .: "next_evolutions"
      return ar

    -- Types
    TypePair a b <- parseJSON ob

    return PokemonPrim {
      _species = s
      , _individual = i
      , _name = name'
      , _statCalc = \case
          HP -> baseHp
          Attack ->  baseAttack
          Defence -> baseDefense
          SpAttack -> baseSattack
          SpDefence -> baseSdefense
          Speed -> baseSpeed
      , _type = (a,b)
      , _weight = weight'
      , _height = height'
      , _catchR = captureRate
      , _bExp = baseExperience
      , _specInfo = isLegendary' ++ isMythical' ++ prevEvolutions ++ nextEvolutions
      }) ob
    
data Stats = Stats (Int,Int,Int,Int,Int,Int) deriving (Show)

statToInt :: Aeson.Key -> Aeson.Value -> Parser Int
statToInt st = withObject "stats" $ \stats -> do
  stat <- stats .: st
  Aeson.Number stat' <- stat .: "base"
  let dbl = toRealFloat stat' :: Double
  return $ floor dbl

instance FromJSON Stats where
  parseJSON = withObject "pokemon" $ \v -> do
    stats <- v .: "stats"
    baseHp <- statToInt "hp" stats
    baseAttack <- statToInt "attack" stats
    baseDefense <- statToInt "defense" stats
    baseSattack <- statToInt "special-attack" stats
    baseSdefense <- statToInt "special-defense" stats
    baseSpeed <- statToInt "speed" stats


    guard $ baseHp > 0 && baseAttack > 0 && baseDefense > 0
    guard $ baseSpeed > 0 && baseSattack > 0 && baseSdefense > 0

    return $ Stats (baseHp, baseAttack, baseDefense, baseSattack, baseSdefense, baseSpeed)

parsePositiveInt :: Aeson.Key -> Aeson.Object -> Parser Int
parsePositiveInt fld obj = do
    Aeson.Number stat' <- obj .: fld
    let dbl = toRealFloat stat' :: Double
    guard $ dbl >= 0
    return $ floor dbl

data TypePair = TypePair Type (Maybe Type) deriving (Show)

instance FromJSON TypePair where
  parseJSON = withObject "pokemon" $ \o -> do
    lst <- slotsAndTypes o >>= onlyValidSlots
    let (x,y) = maybeTuple lst
    (x',y') <- atLeastFstOne (x,y)
    case parseTypesFromText (x',y') of
      (Nothing, Nothing) -> return $ TypePair Normal Nothing
      (Nothing, Just Flying) -> return $ TypePair Normal (Just Flying)
      (Nothing, Just a) -> return $ TypePair a Nothing
      (Just a, Nothing) -> return $ TypePair a Nothing
      (Just a, Just b) -> if a == b then empty else return (TypePair a (Just b))
      

slotsAndTypes :: Aeson.Object -> Parser [(Scientific, Text)]
slotsAndTypes v = do
  Aeson.Array arr <- v .: "types"
  arr' <- forM arr $ \case
    (Object x) -> do
      Aeson.Number slot <- x .: "slot"
      Aeson.String name' <- x .: "name"
      return (slot, name')
    _ -> empty
  return . foldr (:) [] $ arr'

onlyValidSlots :: (Eq n, Num n) => [(n, a)] -> Parser [(n, a)]
onlyValidSlots lst = do
  let vld = filter (\(n,_) -> n `elem` [1,2]) lst
  guard $ length vld == length lst
  return vld

maybeTuple :: Ord n => [(n, a)] -> (Maybe a, Maybe a)
maybeTuple [] = (Nothing, Nothing)
maybeTuple lst =
  listToTuple
  . map (fmap snd . listToMaybe)
  . groupBy (\x y -> fst x == fst y)
  . sortBy (\x y -> compare (fst x) (fst y))
  $ lst
  where
    listToTuple [] = (Nothing,Nothing)
    listToTuple (x:[]) = (x, Nothing)
    listToTuple (x:y:_) = (x, y)

atLeastFstOne :: (Maybe a, Maybe a) -> Parser (a, Maybe a)
atLeastFstOne (Nothing, _) = empty
atLeastFstOne (Just a, b) = return (a, b)

parseTypesFromText :: (Text, Maybe Text)  -> (Maybe Type, Maybe Type)
parseTypesFromText (a, Nothing) = (readText a, Nothing)
parseTypesFromText (a, Just b) = (readText a, readText b)
                                   
parsePhysio :: Aeson.Object -> Aeson.Key -> Parser Double
parsePhysio obj k = do
  phys <- obj .: "physiology"
  Aeson.Number measure <- phys .: k
  return . toRealFloat $ measure
