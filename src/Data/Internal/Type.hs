module Data.Internal.Type where

import Data.List.Extra
import qualified Data.Text as Text
import Text.Read (readMaybe)

data Type =
  Normal
  | Fighting
  | Flying
  | Grass
  | Fire
  | Water
  | Electric
  | Rock
  | Ground
  | Poison
  | Bug
  | Ice
  | Psychic
  | Ghost
  | Dragon
  deriving (Eq,Show,Ord,Enum,Bounded,Read)

readLowerCase :: String -> Maybe Type
readLowerCase "" = Nothing
readLowerCase (x:xs) = readMaybe $ upper [x] ++ xs

readText :: Text.Text -> Maybe Type
readText txt = readLowerCase . Text.unpack . Text.toLower $ txt
