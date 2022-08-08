module Game.PkmnGame where

-- blackAndWhite, blackAndWhite2 :: GameRules

newtype PkmnGame a = PkmnGame (GameRules -> Maybe a)

type GameRules = [PokemonGame]

data PokemonGame

data ChampionsTournament
