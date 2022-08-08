{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Game (
  Game()
  ,twoPlayerGame
  ,twoPlayerRandomizedGame
  ,runStrategy
  ,GamePlay(..)
  ,Randomized(..)
  ,makeMovePool
  ,makeRules
  ,TwoPlayerRandomizedGame
            ) where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Reader

data Game n a h z i = Game
  {
    _chi :: h -> [a]
  , _rho :: PlayerInTurn h n
  , _alpha :: TransitionRule h a z h
  , _pi :: Payoff n z
  , _mapToInformationSet :: QuotientRel h i
  , _h0 :: h
  }

data Manuality =
  Manual
  | Random
  | Automated
  deriving (Eq,Show,Ord,Enum)

twoPlayerGame ::
  Eq n =>
  n -- player 1
  -> n -- player 2
  -> (i -> (a, [a])) -- action function chi, at least one action always available
  -> (i -> Bool) -- is player 1 currently playing, function rho
  -> (h -> a -> Either z h)
  -> Payoff Bool z -- payoff if player 1, else player 2's payoff
  -> QuotientRel h i -- create information sets
  -> h -- h0
  -> Game n a h z i
twoPlayerGame p1 p2 chi rho alpha pi info h0 =
  Game
  {
    _chi = \h -> let
      x = chi . info $ h
      in fst x : snd x
  , _rho = \h -> if rho . info $ h then p1 else p2
  , _alpha = alpha
  , _pi = \n z -> if n == p1 then pi True z else if n == p2 then pi False z else 0
  , _mapToInformationSet = info
  , _h0 = h0
  }

type PureStrategy h a = h -> a

type MixedStrategy h a = h -> a -> Double

type MovePool h hn a an = (h -> (a, [a]), hn -> (an, [an]))

type TransitionRules h hn a an z = (TransitionRule h a z (Either hn h), TransitionRule hn an z (Either hn h))

type TransitionRule h a z nxt = h -> a -> Either z nxt

type Payoff n z = n -> z -> Double

type PlayerInTurn h n = h -> n

type QuotientRel h i = h -> i

type TwoPlayerRandomizedGame n an a hn h z i = Game n (Either an a) (Either hn h) z (Either hn i)

twoPlayerRandomizedGame ::
  Eq n =>
  n
  -> n
  -> MovePool i hn a an
  -> PlayerInTurn i Bool
  -> TransitionRules h hn a an z
  -> Payoff Bool z
  -> QuotientRel h i
  -> Either hn h
  -> Game (Maybe n) (Either an a) (Either hn h) z (Either hn i)
twoPlayerRandomizedGame p1 p2 pool plrturn rules poff quot h0 =
  Game
  {
    _chi = \h -> case h of
      Left x -> map Left . toList $ natureMoves pool x
      Right x -> map Right . toList $ playerMoves pool (quot x)
  , _rho = \h -> case h of
      Left x -> Nothing
      Right x ->  if plrturn . quot $ x then Just p1 else Just p2
  , _alpha = \h a -> case (h,a) of
      (Left x, Left y) -> natureTransit rules x y
      (Right x, Right y) -> playerTransit rules x y
      _ -> Right h
  , _pi = \n z -> case n of
      Nothing -> 0
      Just x ->  if x == p1 then poff True z else if x == p2 then poff False z else 0
  , _mapToInformationSet = \h -> case h of
      Left x -> Left x
      Right x -> Right $ quot x 
  , _h0 = h0
  }

natureMoves :: MovePool h hn a an -> hn -> (an, [an])
natureMoves pool = snd pool

playerMoves ::  MovePool h hn a an -> h -> (a, [a])
playerMoves pool = fst pool

makeMovePool :: (h -> (a, [a])) -> (hn -> (an, [an])) -> MovePool h hn a an
makeMovePool = (,)

natureTransit :: TransitionRules h hn a an z -> hn -> an -> Either z (Either hn h)
natureTransit rules = snd rules

playerTransit :: TransitionRules h hn a an z -> h -> a -> Either z (Either hn h)
playerTransit rules = fst rules

makeRules ::
  (h -> a -> Either z (Either hn h)) -> (hn -> an -> Either z (Either hn h)) -> TransitionRules h hn a an z
makeRules = (,)

toList :: (a, [a]) -> [a]
toList (x,y) = x : y

---

newtype Attempt = Attempt Int deriving (Eq,Ord)

newtype Option = Option String deriving (Eq,Ord)

instance Show Option where
  show (Option x) = show x

firstAttempt :: Attempt
firstAttempt = Attempt 0

nextAttempt :: Attempt -> Attempt
nextAttempt (Attempt a) = Attempt (a + 1)

data GamePlay r where
  AskMove :: Attempt -> [Option] -> GamePlay Option
  Log :: String -> GamePlay ()

makeEffect ''GamePlay

data Randomized r where
  RandomIntegerBetween :: Int -> Int -> Randomized Int
  DrawBetween0And1 :: Randomized Double

makeEffect ''Randomized

-----
  
runStrategy :: (Show a, Read a, Monoid a, Member GamePlay effs, Member Randomized effs)
  => Game n a h z i
  -> (n -> Manuality)
  -> MixedStrategy i a
  -> Eff effs z
runStrategy game manual strat = exec game manual strat (Right (_h0 game))
  where
    exec game manual strat (Left x) = return x
    exec game manual strat (Right x) = do
      let cur = _rho game x -- current player
      let man = manual cur -- manuality
      let act = _chi game x -- actions
      let infs = _mapToInformationSet game x
      decsion <- decide man act strat infs
      exec game manual strat $ _alpha game x decsion

decide :: (Show a, Read a, Monoid a, Member GamePlay effs, Member Randomized effs)
  => Manuality -> [a] -> MixedStrategy i a -> i -> Eff effs a
decide Automated as strat infs = do
  let all = map (strat infs) as -- map actions to probabilities
  let cum = scanl1 (+) all -- create cumulative sum list
  let comb = (,) <$> as <*> cum -- create action & cumulative probability pairs
  draw <- drawBetween0And1
  return $ compare' draw comb
decide Manual as _ _ = do
  let as' = map (Option . show) as
  Option ch <- askMove firstAttempt as'
  return $ read ch
decide Random as _ _ = do
  let l = fromIntegral $ length as
  let m = map ((\x -> x / l) . const 1.0) as
  let c = (,) <$> as <*> m
  draw <- drawBetween0And1
  return $ compare' draw c
  
compare' _ [] = mempty
compare' x ((a,prob):as) = if x < prob then a else compare' x as
