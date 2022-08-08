{-# LANGUAGE TemplateHaskell  #-}

module Domain.Battle.Pokemon where

import Control.Lens
import Domain.Battle.List

class Speed a where
  speed :: a -> Double

class Range a where
  range :: a -> Int

type NameOf = String
type HP = Int
type Coord = (Int,Int)

------------------------------------------
----------- Pokemon ----------------------
------------------------------------------

data Pokemon n pkmn mv it = Pokemon
  {
    _nameOfPokemon :: NameOf
  , _trainerOfPokemon :: n
  , _pokemonState :: pkmn
  , _pokemonHP :: HP
  , _pokemonMoves :: Moves (Move n mv pkmn it)
  , _pokemonItem :: Maybe it
  , _pokemonPosition :: Coord
  } deriving (Eq,Show,Ord)

instance Speed pkmn => Speed (Pokemon n pkmn mv it) where
  speed p = f p
    where
      f = speed . _pokemonState

-----------------------------------------
-------------- Moves --------------------
-----------------------------------------

data Move n mv pkmn it = Move
  {
    _moveState :: mv
  , _movePP :: Int
  , _userOfMove :: Pokemon n pkmn mv it
  }


instance (Show n, Show mv, Show pkmn, Show it) => Show (Move n mv pkmn it) where
  show (Move s p u) = "Move " ++ show s ++ " " ++ show p ++ " " ++ show (_nameOfPokemon u)

instance (Eq n, Eq mv, Eq pkmn, Eq it) => Eq (Move n mv pkmn it) where
  Move s p u == Move s' p' u' = s == s' && p == p'

instance Speed mv => Speed (Move n mv pkmn it) where
  speed mv = speed . _moveState $ mv

instance (Eq n, Eq mv, Eq pkmn, Eq it, Speed mv, Speed pkmn) => Ord (Move n mv pkmn it) where
  m <= n = (speed m, speed . _userOfMove $ m) <= (speed n, speed . _userOfMove $ n)


makeLenses ''Pokemon
makeLenses ''Move

instance Range mv => Range (Move n mv pkmn it) where
  range m = m ^. moveState & range


