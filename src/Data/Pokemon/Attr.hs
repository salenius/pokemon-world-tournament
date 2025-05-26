{-# LANGUAGE TypeFamilies, OverloadedStrings, LambdaCase #-}

module Data.Pokemon.Attr (
  Attr(), fromJson,
  PkmnStat(),
  Flag(),
  Rate(),
  asInt,
  isTrue, isFalse,
  AttrAssign(..), StatSym(..), FlagSym(..),
  PhysioSym(..), TypeSym(..), RateSym(..) 
  
                         ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Types.GenI
import Data.Stats
import Data.Units
import qualified Data.ByteString.Lazy as BS
import Data.Scientific (toRealFloat)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List (groupBy, sortBy)

{-

Copyright (C) Tommi Salenius, 2025

General attributes that all Pokemon share across the
games. These can be grouped into five separate groups:
- stats
- physio
- types
- rates
- flags

These are general values which technically can change
over the game cycle, and which do not affect the Pokemon's
identity.

Following laws should be noted:

- all the setters dealing with numerical attributes
only return strictly positive values and at the maximum 255
- isMythical x == True => isLegendary x == True
- isLegendary x == False => isMythical x == False
- a <~ special x == a <~ spAttack x <~ sDefense x
- weight x <= Kg 1000
- height x <= Meters 100
- isLegendary x == False => sum (map (asInt . ($ x)) [hp,attack,defense,spAttack,spDefense,speed]) <= 670

-}


data Attr = Attr {
  _hp :: Int
  , _attack :: Int
  , _defence :: Int
  , _spAttack :: Int
  , _spDefence :: Int
  , _speed :: Int
  , _staticHP :: Bool
  , _type1 :: Type
  , _type2 :: Maybe Type
  , _isLegendary :: Bool
  , _isMythical :: Bool
  , _evolvesFurther :: Bool
  , _evolvedFurther :: Bool
  , _baseExp :: Int
  , _catchRate :: Int
  , _weight :: Kg
  , _height :: Meters
  }

fromJson :: BS.ByteString -> Maybe Attr
fromJson = Aeson.decode

instance FromJSON Attr where
  parseJSON = let
    -- catch rate and base experience parsed into integer values
    parsePositiveInt fld obj = do
      Aeson.Number stat' <- obj .: fld
      let dbl = toRealFloat stat' :: Double
      guard $ dbl >= 0
      return $ floor dbl
    -- weight and height parsed into floats
    parsePhysio obj k = do
      phys <- obj .: "physiology"
      Aeson.Number measure <- phys .: k
      return . toRealFloat $ measure
    -- base stats parsed into integer values
    statToInt st = withObject "stats" $ \stats -> do
      stat <- stats .: st
      Aeson.Number stat' <- stat .: "base"
      let dbl = toRealFloat stat' :: Double
      return $ floor dbl

    -- types parsed from text into GenI types.
    -- The next five functions are composed together
    -- to accomplish the goal
    slotsAndTypes v = do
      Aeson.Array arr <- v .: "types"
      arr' <- forM arr $ \case
        (Aeson.Object x) -> do
          Aeson.Number slot <- x .: "slot"
          Aeson.String name' <- x .: "name"
          return (slot, name')
        _ -> empty
      return . foldr (:) [] $ arr'

    onlyValidSlots lst = do
      let vld = filter (\(n,_) -> n `elem` [1,2]) lst
      guard $ length vld == length lst
      return vld

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

    atLeastFstOne (Nothing, _) = empty
    atLeastFstOne (Just a, b) = return (a, b)

    parseTypesFromText (a, Nothing) = (readText a, Nothing)
    parseTypesFromText (a, Just b) = (readText a, readText b)


    in withObject "pokemon" $ \v -> do
        -- Stats
        stats <- v .: "stats"
        baseHp <- statToInt "hp" stats
        baseAttack <- statToInt "attack" stats
        baseDefense <- statToInt "defense" stats
        baseSattack <- statToInt "special-attack" stats
        baseSdefense <- statToInt "special-defense" stats
        baseSpeed <- statToInt "speed" stats


        guard $ baseHp > 0 && baseAttack > 0 && baseDefense > 0
        guard $ baseSpeed > 0 && baseSattack > 0 && baseSdefense > 0

        -- Weight and height
        weight' <- kg <$> parsePhysio v "weight"
        height' <- meters <$> parsePhysio v "height"

        species <- v .: "species"

        -- Catch rate and base experience
        captureRate <- parsePositiveInt "capture_rate" species

        baseExperience <- parsePositiveInt "base_experience" v

        -- Flags
        isLegendary' <- species .: "is_legendary"
        isMythical' <- species .: "is_mythical"
        Aeson.Array evols <- species .: "next_evolutions"
        let nextEvolutions = (> 0) . length $ evols

        -- Types
        lst <- slotsAndTypes v >>= onlyValidSlots
        let (x,y) = maybeTuple lst
        (x',y') <- atLeastFstOne (x,y)
        (typenum1, typenum2) <- case parseTypesFromText (x',y') of
          (Nothing, Nothing) -> return $ (Normal, Nothing)
          (Nothing, Just Flying) -> return $ (Normal, Just Flying)
          (Nothing, Just a) -> return $ (a, Nothing)
          (Just a, Nothing) -> return $ (a, Nothing)
          (Just a, Just b) -> if a == b then empty else return (a, Just b)

        return $ Attr {
           _hp = baseHp
         , _attack = baseAttack
         , _defence = baseDefense
         , _spAttack = baseSattack
         , _spDefence = baseSdefense
         , _speed = baseSpeed
         , _staticHP = False
         , _type1 = typenum1
         , _type2 = typenum2
         , _isLegendary = isLegendary'
         , _isMythical = isMythical'
         , _evolvesFurther = nextEvolutions
         , _evolvedFurther = False
         , _baseExp = baseExperience
         , _catchRate = captureRate
         , _weight = weight'
         , _height = height'
  
        }
    

data PkmnStat =
  Stat Stat Int
  deriving (Eq,Ord)

data Flag = Flag Flag' Bool deriving (Eq,Ord)

data Flag' =
  Legendary
  | Mythical
  | NextEvols
  | PrevEvols
  deriving (Eq,Ord)

data Rate =
  CatchRate Int
  | BaseExp Int
  deriving (Eq,Ord)

class AttrAssign a where
  (<~) :: Attr -> a -> Attr

instance AttrAssign PkmnStat where
  a <~ x = 
    let stat = case x of
          Stat HP y -> s1 . validate _hp $ y
          Stat Attack y -> s2 . validate _attack $ y
          Stat Defence y -> s3 . validate _defence $ y
          Stat SpAttack y -> s4 . validate _spAttack $ y
          Stat SpDefence y -> s5 . validate _spDefence $ y
          Stat Speed y -> s6 . validate _speed $ y
        validate f = shouldBe a f
        shouldBe attr getter statVal = if statVal > 0 && statVal < 256 then statVal else getter attr
        s1 b = a {_hp = b}
        s2 b = a {_attack = b}
        s3 b = a {_defence = b}
        s4 b = a {_spAttack = b}
        s5 b = a {_spDefence = b}
        s6 b = a {_speed = b}
    in stat

instance AttrAssign Kg where
  a <~ w = a {_weight = w}

instance AttrAssign Meters where
  a <~ h = a {_height = h}

instance AttrAssign Flag where
  a <~ Flag f b = case f of
    Legendary -> a {_isLegendary = b}
    Mythical  -> a {_isMythical = b}
    NextEvols -> a {_evolvesFurther = b}
    PrevEvols -> a {_evolvedFurther = b}

instance AttrAssign Rate where
  a <~ CatchRate x = a {_catchRate = if x > 0 then x else _catchRate a}
  a <~ BaseExp x = a {_baseExp = if x > 0 then x else _baseExp a}

class StatSym a where
  hp, attack, defense, spAttack, spDefense, speed :: a -> PkmnStat

instance StatSym Int where
  hp = Stat HP
  attack = Stat Attack
  defense = Stat Defence
  spAttack = Stat SpAttack
  spDefense = Stat SpDefence
  speed = Stat Speed

instance StatSym Attr where
  hp = Stat HP . _hp
  attack = Stat Attack . _attack
  defense = Stat Defence . _defence
  spAttack = Stat SpAttack . _spAttack
  spDefense = Stat SpDefence . _spDefence
  speed = Stat Speed . _speed

class FlagSym a where
  isLegendary, isMythical, hasPrevEvols, hasNextEvols :: a -> Flag

instance FlagSym Bool where
  isLegendary = Flag Legendary
  isMythical = Flag Mythical
  hasPrevEvols = Flag PrevEvols
  hasNextEvols = Flag NextEvols

instance FlagSym Attr where
  isLegendary = isLegendary . _isLegendary
  isMythical = isMythical . _isMythical
  hasPrevEvols = hasPrevEvols . _evolvedFurther
  hasNextEvols = hasNextEvols . _evolvesFurther


asInt :: PkmnStat -> Int
asInt (Stat _ x) = x

isTrue, isFalse :: Flag -> Bool
isTrue (Flag _ x) = x
isFalse (Flag _ x) = not x

class TypeSym a where
  fstType :: a -> Type
  sndType :: a -> Maybe Type

instance TypeSym Type where
  fstType = id
  sndType = Just

class PhysioSym a where
  weight :: a -> Kg
  height :: a -> Meters

instance PhysioSym Double where
  weight x = oneKg * kg x
  height x = oneMeter * meters x

class RateSym a where
  baseExp, catchRate :: a -> Rate


