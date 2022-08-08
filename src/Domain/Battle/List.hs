{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}


module Domain.Battle.List where

import qualified Data.Set as Set

data Party pkmn =
  PartyOf1 pkmn
  | PartyOf2 pkmn pkmn 
  | Party (Party pkmn) (Party pkmn) (Party pkmn)
  deriving (Show,Ord,Foldable,Functor)

instance (Ord pkmn, Eq pkmn) => Eq (Party pkmn) where
  p == q = toList p == toList q

data Moves mv =
  OneMove mv
  | TwoMoves mv mv
  | ThreeMoves mv mv mv
  | FourMoves mv mv mv mv
  deriving (Show,Ord,Foldable,Functor)


instance (Ord pkmn, Eq pkmn) => Eq (Moves pkmn) where
  p == q = toList p == toList q

toList :: (Foldable p, Eq a, Ord a) => p a -> Set.Set a
toList = Set.fromList . foldr (:) []

