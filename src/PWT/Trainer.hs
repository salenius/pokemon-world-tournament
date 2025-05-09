{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module PWT.Trainer where

import Data.Bifunctor
import Data.Text

data Trainer pkmn it = Trainer
  {
    name :: Text
  , trainerClass :: Class
  , wealth :: Money
  , party :: Party pkmn
  , itemBag :: Bag it
  } deriving (Eq,Show,Ord,Functor)

instance Bifunctor Trainer where
  bimap f g (Trainer n t w p i) = Trainer n t w (fmap f p) (fmap g i)

introduce :: Trainer pkmn it -> Text
introduce trainer = className (trainerClass trainer) <> " " <> name trainer

type Bag a = [a]

data Party a =
  P1 a
  | P2 a a
  | P3 a a a
  | P4 a a a a
  | P5 a a a a a
  | P6 a a a a a a
  deriving (Eq,Show,Ord,Functor,Foldable,Traversable)

newtype Money = Money Int deriving (Eq,Show,Ord,Num)

newtype Class = Class {className :: Text} deriving (Eq,Show,Ord)
