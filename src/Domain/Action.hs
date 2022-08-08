{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}


module Domain.Action where

import Domain.Battle.Pokemon

data BattlePhase =
  Metagame
  | BattleProper
  deriving (Eq,Show,Ord,Enum)

data Action n pkmn mv it eff where
  UseMove ::
    Pokemon n pkmn mv it
    -> Move n mv pkmn it
    -> Pokemon n pkmn mv it
    -> Action n pkmn mv it eff
  SwitchPokemon ::
    Pokemon n pkmn mv it
    -> Pokemon n pkmn mv it
    -> Action n pkmn mv it eff
  UseItem ::
    it
    -> Pokemon n pkmn mv it
    -> Action n pkmn mv it eff
  Resign ::
    n
    -> Action n pkmn mv it eff
  CombineActions ::
    Action n pkmn mv it eff
    -> Action n pkmn mv it eff
    -> Action n pkmn mv it eff
  AddEffect ::
    Action n pkmn mv it eff
    -> eff
    -> Action n pkmn mv it eff
  PureEffect ::
    eff
    -> Action n pkmn mv it eff
  -- This case handles Pursuit
  UseHighspeedMove ::
    Pokemon n pkmn mv it
    -> Move n mv pkmn it
    -> Pokemon n pkmn mv it
    -> Action n pkmn mv it eff

instance Functor (Action n pkmn mv it) where
  fmap _ (UseMove a b c) = UseMove a b c
  fmap _ (SwitchPokemon a b) = SwitchPokemon a b
  fmap _ (UseItem a b) = UseItem a b
  fmap _ (Resign n) = Resign n
  fmap f (CombineActions a b) = CombineActions (fmap f a) (fmap f b)
  fmap f (AddEffect a b) = AddEffect (fmap f a) (f b)
  fmap f (PureEffect a) = PureEffect . f $ a
  fmap _ (UseHighspeedMove a b c) = UseHighspeedMove a b c

deriving instance (Show n, Show pkmn, Show mv, Show it, Show eff) => Show (Action n pkmn mv it eff)
deriving instance (Eq n, Eq pkmn, Eq mv, Eq it, Eq eff, Speed mv, Speed pkmn) => Eq (Action n pkmn mv it eff)

instance (Ord n, Ord pkmn, Ord mv, Ord it, Ord eff, Speed mv, Speed pkmn) => Ord (Action n pkmn mv it eff) where
  _ <= Resign _ = True
  UseHighspeedMove _ m _ <= UseHighspeedMove _ n _ = m <= n
  _ <= UseHighspeedMove _ _ _ = True
  SwitchPokemon p _ <= SwitchPokemon q _ = speed p <= speed q
  _ <= SwitchPokemon _ _ = True
  _ <= UseItem _ _ = True
  UseMove _ m _ <= UseMove _ n _ = m <= n
  _ <= UseMove _ _ _ = True
  CombineActions a _ <= CombineActions b _ = a <= b
  CombineActions a _ <= b = a <= b
  PureEffect e <= PureEffect f = e <= f
  PureEffect _ <= _ = True

instance Semigroup (Action n pkmn mv it eff) where
  a <> b = CombineActions a b

instance Monoid eff => Monoid (Action n pkmn mv it eff) where
  mempty = PureEffect mempty

type Party a = (a, Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)
  
makeHighspeed :: Action n pkmn mv it eff -> Action n pkmn mv it eff
makeHighspeed = remapActions f
  where
    f (UseMove a b c) = UseHighspeedMove a b c
    f x = x

remapActions :: (Action n pkmn mv it eff -> Action n pkmn mv it eff)
  -> Action n pkmn mv it eff -> Action n pkmn mv it eff
remapActions f (CombineActions a b) = CombineActions (remapActions f a) (remapActions f b)
remapActions f (AddEffect a b) = AddEffect (remapActions f a) b
remapActions f x = f x
