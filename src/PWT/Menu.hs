-- | 
{-# LANGUAGE ViewPatterns #-}

module PWT.Menu (
  menuRanger,
  runApplication,
  initMenu
                ) where

import Data.Functor.Foldable (ana)
import Data.Functor.Base
import Data.Tree
import Control.Arrow ((&&&))
import Data.Maybe (isNothing)


type EitherBattleSpec p = BattleSpecs (Either (p, p) ((p, p), (p, p)))

data Menu p =
  MainMenu !OpenedNewMenu !OptionState
  | OptionsMenu !OptionState
  | GenerationPickMenu !OptionState
  | InteractiveBattleMenu !OptionState
  | NewBattleMenu !OptionState
  | BattleFormatMenu !OptionState
  | TournamentMenu !OptionState
  | ChoosePlayerMenu (BattleSpecs [p])
  | ChooseTagBattlePlayerMenu (BattleSpecs [p])
  | BattlePrepMenu (EitherBattleSpec p)
  | FightModeMenu (EitherBattleSpec p)
  | WeatherChangeMenu (EitherBattleSpec p)
  | TerrainChangeMenu (EitherBattleSpec p)
  | QuitMenu
  deriving (Eq,Show,Ord)

data SingleFormat =
  Singles
  | Doubles
  | Triples
  deriving (Eq,Show,Ord,Enum)

data BattleSpecs a = BattleSpecs
  {
    optionState :: OptionState
  , battleFormatChosen :: Maybe SingleFormat
  , playerChoices :: a
  , weather :: Maybe Weather
  } deriving (Eq,Show,Ord)

type OpenedNewMenu = Bool

data OptionState = OptionState
  {
    generationChosen :: Generation
  , battleIsInteractive :: Bool
  } deriving (Eq,Show,Ord)

data Weather =
  Clear
  | Sunny
  | Rainy
  | Sandstorm
  | Hail
  deriving (Eq,Show,Ord,Enum,Bounded)

data Generation =
  GenV
  | GenVI
  deriving (Eq,Show,Ord,Enum,Bounded)

data MenuSetting =
  MenuSetting !Title [String] deriving (Eq,Show,Ord)

instance Semigroup MenuSetting where
  MenuSetting a b <> MenuSetting c d = MenuSetting (a <> c) (b <> d)

instance Monoid MenuSetting where
  mempty = MenuSetting mempty mempty

type Title = String


runApplication :: TreeF a (IO ()) -> IO ()
runApplication = undefined

optionsAvailable :: Menu p -> [Menu p]
optionsAvailable (MainMenu _ opt)               = [NewBattleMenu opt, OptionsMenu opt, QuitMenu]
optionsAvailable QuitMenu                       = []
optionsAvailable (OptionsMenu opt)              = [GenerationPickMenu opt, InteractiveBattleMenu opt, mainMenu opt]
optionsAvailable (GenerationPickMenu opt)       = fmap (\gen -> OptionsMenu opt {generationChosen = gen}) [GenV, GenVI]
optionsAvailable (InteractiveBattleMenu opt)    = [OptionsMenu opt {battleIsInteractive = True},
                                                   OptionsMenu opt {battleIsInteractive = False}]
optionsAvailable (NewBattleMenu opt)            = [BattleFormatMenu opt, TournamentMenu opt, mainMenu opt]
optionsAvailable (BattleFormatMenu opt)         = fmap f [Singles, Doubles, Triples] ++ other
  where
    f t   = ChoosePlayerMenu (BattleSpecs opt (Just t) [] (Just Clear))
    other = [ChooseTagBattlePlayerMenu (BattleSpecs opt Nothing [] (Just Clear)), NewBattleMenu opt]
optionsAvailable (ChoosePlayerMenu _)          = [] -- Will be filled later
optionsAvailable (ChooseTagBattlePlayerMenu _) = [] -- Will be filled later
optionsAvailable (BattlePrepMenu spc)          = [FightModeMenu spc, WeatherChangeMenu spc, TerrainChangeMenu spc]
optionsAvailable (FightModeMenu rst)           = [mainMenu (optionState rst), FightModeMenu rst]
optionsAvailable (TournamentMenu opt)          = [mainMenu opt]
optionsAvailable (WeatherChangeMenu src)       =
  fmap (\x -> BattlePrepMenu src {weather = x}) . flip (++) [Nothing] . fmap Just $ enumFrom minBound
optionsAvailable (TerrainChangeMenu src)       = [BattlePrepMenu src] -- TODO

trainerOptions :: Show p => [p] -> Menu p -> [Menu p]
trainerOptions trainers (ChoosePlayerMenu s)
  | Prelude.null $ playerChoices s                    = fmap (\p -> ChoosePlayerMenu s {playerChoices = p : mempty}) trainers
  | otherwise                                         = fmap battlePrep trainers
  where
    battlePrep p = BattlePrepMenu s {playerChoices = Left (p, Prelude.head . playerChoices $ s)}
trainerOptions _        (ChooseTagBattlePlayerMenu s) = [BattleFormatMenu (optionState s)] -- OPTIONS for TRAINERS added later
trainerOptions _        _                             = mempty


optionDescription :: Show p => Menu p           -> Menu p                                       -> String
optionDescription (MainMenu _ _)                (NewBattleMenu _)                               = "New Battle"
optionDescription (MainMenu _ _)                (OptionsMenu _)                                 = "Options"
optionDescription (MainMenu _ _)                QuitMenu                                        = "Quit"
optionDescription (OptionsMenu _)               (GenerationPickMenu _)                          = "Choose a Pokemon Generation"
optionDescription (OptionsMenu _)               (InteractiveBattleMenu _)                       = "Set the battle interactivity"
optionDescription (OptionsMenu _)               (MainMenu _ _)                                  = "Return to the Main Menu"
optionDescription (GenerationPickMenu _)        (OptionsMenu (generationChosen -> GenV))        = "Generation V" 
optionDescription (GenerationPickMenu _)        (OptionsMenu (generationChosen -> GenVI))       = "Generation VI" 
optionDescription (InteractiveBattleMenu _)     (OptionsMenu (battleIsInteractive -> True))     = "Yes, I want to battle!"
optionDescription (InteractiveBattleMenu _)     (OptionsMenu (battleIsInteractive -> False))    = "No, I just want to watch battles"
optionDescription (NewBattleMenu _)             (BattleFormatMenu _)                            = "Single Battle"
optionDescription (NewBattleMenu _)             (TournamentMenu _)                              = "Tournament"
optionDescription (NewBattleMenu _)             (MainMenu _ _)                                  = "Return to Main Menu"
optionDescription (BattleFormatMenu _)          (ChoosePlayerMenu s)
  | battleFormatChosen s == Just Singles                                                        = "Single Battle"
  | battleFormatChosen s == Just Doubles                                                        = "Double Battle"
  | battleFormatChosen s == Just Triples                                                        = "Triple Battle"
optionDescription (BattleFormatMenu _)          (ChooseTagBattlePlayerMenu s)
  | isNothing $ battleFormatChosen s                                                            = "Tag Battle"
optionDescription (BattleFormatMenu _)          (NewBattleMenu _)                               = "Go back"
optionDescription (ChoosePlayerMenu s)          (ChoosePlayerMenu (playerChoices -> p:_))
  | Prelude.null $ playerChoices s                                                              = show p 
optionDescription (ChoosePlayerMenu s)          (BattlePrepMenu (playerChoices -> Left (_, p)))
  | not . Prelude.null $ playerChoices s                                                        = show p
optionDescription (BattlePrepMenu _)            (FightModeMenu _)                               = "No, let's battle!"
optionDescription (BattlePrepMenu _)            (WeatherChangeMenu _)                           = "Change Weather"
optionDescription (BattlePrepMenu _)            (TerrainChangeMenu _)                           = "Change Terrain"
optionDescription (WeatherChangeMenu _)         (BattlePrepMenu (weather -> Just w))            = show w
optionDescription (WeatherChangeMenu _)         (BattlePrepMenu (weather -> Nothing))           = "Randomized Weather"
optionDescription (TerrainChangeMenu _)         (BattlePrepMenu _)                              = "Return" -- implemented later
optionDescription (TournamentMenu _)            (NewBattleMenu _)                               = "Return" -- implemented later
optionDescription (FightModeMenu _)             (MainMenu _ _)                                  = "Main Menu"
optionDescription (FightModeMenu _)             (FightModeMenu _)                               = "Rematch"
optionDescription (ChooseTagBattlePlayerMenu _) (BattleFormatMenu _)                            = "Return" -- implemented later 
optionDescription _                             _                                               = ""

optionsWithDescription :: Show p => [p] -> Menu p -> [(String, Menu p)]
optionsWithDescription trainers menu =
  Prelude.filter ((/=) "" . fst)
  . fmap (optionDescription menu &&& id)
  $ optionsAvailable menu <> trainerOptions trainers menu

menuTitle :: Show p => Menu p -> Title
menuTitle (MainMenu False _)            = "Welcome to play Pokemon World Tournament simulator!"
menuTitle (MainMenu True _)             = "Pokemon World Tournament - Main Menu"
menuTitle (OptionsMenu _)               = "Options"
menuTitle (GenerationPickMenu _)        = "Choose a Pokemon generation"
menuTitle (InteractiveBattleMenu _)     = "Do you want to battle or just watch battles?"
menuTitle (NewBattleMenu _)             = "Choose how do you want to battle"
menuTitle (TournamentMenu _)            = "What kind of tournament do you want to play?"
menuTitle (BattleFormatMenu _)          = "Choose the battle format"
menuTitle (ChoosePlayerMenu s)
  | Prelude.null $ playerChoices s      = playerChoiceText . optionState $ s
  | otherwise                           = opponentChoiceText (optionState s) . Prelude.head . playerChoices $ s
menuTitle (BattlePrepMenu _)            = "Do you want to further modify specs for the battle?"
menuTitle (FightModeMenu _)             = "Return to Main Menu or take a rematch?"
menuTitle (WeatherChangeMenu _)         = "Specify a weather for the battle or play lottery"
menuTitle (TerrainChangeMenu _)         = "Specify a terrain for the battle or play lottery"
menuTitle (ChooseTagBattlePlayerMenu s)
  | Prelude.null $ playerChoices s      =  playerChoiceText . optionState $ s
  | playersChosen 1 && interactive      = "Choose your team mate"
  | playersChosen 1                     = "Choose a team mate for player 1"
  | playersChosen 2 && interactive      = "Choose your opponent"
  | playersChosen 2                     = "Choose an opponent for player 1"
  | playersChosen 3 && interactive      = "Choose your opponen'ts team mate"
  | otherwise                           = "Choose team mate for player 3"
  where
    interactive     = battleIsInteractive . optionState $ s
    playersChosen n = length (playerChoices s) == n
menuTitle QuitMenu                      = ""

--

menuRanger :: (Show p) => [p] -> Menu p -> TreeF MenuSetting (Menu p)
menuRanger trainers menu = nextMenu (menuTitle menu) (optionsWithDescription trainers menu) 

initMenu :: Menu p
initMenu = MainMenu False (OptionState GenV True)

nextMenu :: Title -> [(String, b)] -> TreeF MenuSetting b
nextMenu text showing = NodeF (MenuSetting text (fmap fst showing)) (fmap snd showing)

mainMenu :: OptionState -> Menu p
mainMenu = MainMenu False

playerChoiceText :: OptionState -> String
playerChoiceText (battleIsInteractive -> True) = "Choose a trainer as your champion"
playerChoiceText _ = "Choose Player no 1"

opponentChoiceText :: Show p => OptionState -> p -> String
opponentChoiceText (battleIsInteractive -> True) _ = "Choose your opponent"
opponentChoiceText _                             p = "Choose Player no 2 as an opponent for " <> show p

-- DO NOT run in GHCi, just to show here
example :: Tree MenuSetting
example = ana (menuRanger ["Red","Lance"]) initMenu


