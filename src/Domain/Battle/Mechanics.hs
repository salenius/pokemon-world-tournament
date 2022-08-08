{-# LANGUAGE MultiParamTypeClasses #-}

module Domain.Battle.Mechanics where

import Domain.Battle.Internal
import Domain.Battle.List
import Domain.Battle.Pokemon
import Control.Lens hiding (Choice)

type MetagameChoices n pkmn mv it = Metagame n pkmn mv it -> (PlayerState n pkmn mv it, [PlayerState n pkmn mv it])

type PlayerChoices n pkmn mv it eff = Battlefield n pkmn mv it eff n -> [Choice n mv pkmn it]

-------------------------------------
--- Perfect knowledge about metagame
-------------------------------------

class (Eq n, Eq pkmn, Eq mv, Eq it, Ord mv, Ord n, Ord pkmn, Ord it, Speed mv, Speed pkmn) => Prioritizable n pkmn mv it

perfectKnowledgeMeta :: (n -> PlayerState n pkmn mv it) -> MetagameChoices n pkmn mv it
perfectKnowledgeMeta getter meta = (getter $ field ^. whoseTurn, []) 
  where
    field = metagameToBattlefield meta

mkPerfectKnowledgeMeta ::
  Prioritizable n pkmn mv it
  => (n -> Party (Pokemon n pkmn mv it))
  -> (n -> [it])
  -> (n -> PlayerState n pkmn mv it)
mkPerfectKnowledgeMeta f g n = lostTrainer (\m pt it -> if m == n && pt == f n && it == g n then 1 else 0)

--------------------------------------
---- Defining choices for players
--------------------------------------

type FieldRange = Int

