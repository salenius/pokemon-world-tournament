module Logic.Battle.Core where

import Control.Monad.State
import Data.List

data Team = Team1 | Team2 deriving (Eq,Show,Ord,Enum,Bounded)
data StartTurn = StartTurn
data EndTurn = EndTurn
data Choice plr mv it pkmn = ExecMove mv [plr] | UseItem it plr | SwitchPokemon pkmn deriving (Eq,Show,Ord)
type Choices plr mv it pkmn = plr -> Choice plr mv it pkmn
data Success = NonSuccessful | Successful deriving (Eq,Show,Ord,Enum,Bounded)
type Cont r a = (a -> r) -> r
data Finished = Winner Team deriving (Eq,Show,Ord)

type Battle m b = StateT b m 
type OrderDeterminant plr = plr -> OrderNum
data OrderNum = OrderNum { specialPriority :: Int, movePriority :: Int, speedPriority :: Int} deriving (Eq,Show,Ord)

data Skip s = DoNotSkip s | Skip deriving (Eq,Show,Ord)

type InTeam plr = plr -> Team

type DamageDone = Int

data StatusHit = StatusDidntHit | StatusHit deriving (Eq,Show,Ord,Enum)

data AttackHit = AttackDidntHit | AttackHit DamageDone deriving (Eq,Show,Ord)

data UserTargetPair plr = UserTargetPair { userWas :: plr, targetWas :: [plr]} deriving (Eq,Show,Ord)

data ExecData plr =
  ExecutionFailed plr 
  | UsedItem plr 
  | SwitchedPokemon plr 
  | ExecutedStatusMove (UserTargetPair plr, StatusHit) 
  | ExecutedAttackingMove (UserTargetPair plr, AttackHit) 
  deriving (Eq,Show)

data ExecProcess plr =
  ProcessIncomplete (ExecData plr)
  | AllExecuted
  deriving (Eq,Show)

----

type BattleCont m b a = Cont (Battle m b Finished) a

type PriorSuccessData plr mv it pkmn = (Choices plr mv it pkmn, OrderDeterminant plr, ExecData plr)
type SuccessData plr mv it pkmn = (Choices plr mv it pkmn, OrderDeterminant plr, ExecData plr, plr)

type UserDeterminationData plr mv it pkmn = (Choices plr mv it pkmn, OrderDeterminant plr, [ExecData plr])
type MaybeSkipExecutionData plr mv it pkmn = (Choices plr mv it pkmn, OrderDeterminant plr, [ExecData plr], Skip plr)
type AliveCheckData plr mv it pkmn = (Choices plr mv it pkmn, OrderDeterminant plr, [ExecData plr], plr)
type PreExecutionData plr mv it pkmn = (Choices plr mv it pkmn, OrderDeterminant plr, [ExecData plr], plr)

---- User has to implement logic for these types
type ChoiceMaker m b plr mv it pkmn = Battle m b (Choices plr mv it pkmn)
type OrderMaker m b plr mv it pkmn = Choices plr mv it pkmn -> Battle m b (OrderDeterminant plr)
type MoveExec m b plr mv it pkmn = mv -> PreExecutionData plr mv it pkmn -> BattleCont m b (ExecData plr)
type ItemExec m b plr mv it pkmn = it -> PreExecutionData plr mv it pkmn -> BattleCont m b (ExecData plr)
type SwitchExec m b plr mv it pkmn = pkmn -> PreExecutionData plr mv it pkmn -> BattleCont m b (ExecData plr)

----

-- 1) This function determines what players choose and in which order choices are executed
choosingAndOrdering ::
  Monad m =>
  ChoiceMaker m b plr mv it pkmn
  -> OrderMaker m b plr mv it pkmn
  -> StartTurn
  -> BattleCont m b (Choices plr mv it pkmn, OrderDeterminant plr)
choosingAndOrdering makeChoices determineFaster _ exec = do
  chs <- makeChoices
  fst <- determineFaster chs
  exec (chs, fst)

-- startExecutingChoices = willExecChoices (ProcessIncomplete [])



-- 2) Determine user next if this is okay. If no more executions left,
-- skip to the end of the turn.
nextExecutionWithUser :: (Monad m, Ord plr, Enum plr) =>
  UserDeterminationData plr mv it pkmn
  -> BattleCont m b (MaybeSkipExecutionData plr mv it pkmn)
nextExecutionWithUser (chs, od, exd) = \exec ->
  let
    user = determineUserMaybe od exd
  in case user of
    Just usr -> exec (chs, od, exd, DoNotSkip usr)
    Nothing -> exec (chs, od, exd, Skip)

determineUserMaybe :: (Ord plr, Enum plr) => OrderDeterminant plr -> [ExecData plr] -> Maybe plr
determineUserMaybe od exd =
  let
    players = enumFrom . toEnum $ 0
    orders = map od players
    playersSorted = map snd . reverse . sort . zip orders $ players
  in if length exd >= length players
     then Nothing
     else Just (playersSorted !! length exd)

-- 2.5) Propagate program to go the end of the turn, if it was determined in such way the last turn
skipEndOfTurnIfNecessary :: Monad m =>
  Battle m b Finished
  -> MaybeSkipExecutionData plr mv it pkmn
  -> BattleCont m b (AliveCheckData plr mv it pkmn)
skipEndOfTurnIfNecessary endOfTurn (chs, od, exd, skips) = \exec ->
  case skips of
    DoNotSkip usr -> exec (chs, od, exd, usr)
    _             -> endOfTurn

-- 2.5.5) Check that the user is alive, so that it can attack
userCanExecute :: Monad m =>
  (plr -> b -> Bool)
  -> (UserDeterminationData plr mv it pkmn -> Battle m b Finished)
  -> AliveCheckData plr mv it pkmn
  -> BattleCont m b (PreExecutionData plr mv it pkmn)
userCanExecute checkAlive goBackToUserDetermination (chs, od, exd, usr) = \exec ->
  case chs usr of
    SwitchPokemon _ -> exec (chs, od, exd, usr)
    _ -> do
      s <- getBattle
      let al = checkAlive usr s
      if al then exec (chs, od, exd, usr) else goBackToUserDetermination (chs, od, exd <> [ExecutionFailed usr])
    
-- 3) Specialize in execution depending on which is taken
specializeInChoice :: Monad m =>
  MoveExec m b plr mv it pkmn
  -> ItemExec m b plr mv it pkmn
  -> SwitchExec m b plr mv it pkmn
  -> PreExecutionData plr mv it pkmn
  -> BattleCont m b (UserDeterminationData plr mv it pkmn)
specializeInChoice moveExecution itemExecution switchExecution (chs, od, exd, usr) = \exec ->
  let
    exec' = exec . ((,,) chs od) . (exd ++) . return
    args = (chs, od, exd, usr)
    in
  case chs usr of
    ExecMove mv plrs -> moveExecution mv args exec'
    UseItem it plr -> itemExecution it args exec'
    SwitchPokemon pkmn -> switchExecution pkmn args exec'

-- 4) End turn
-- endTurn battleOverCheck turnEndingOp = \exec -> do
  -- turnEndingOp
  -- isFinished battleOverCheck exec

-- isFinished battleOverCheck = \exec ->
  -- b <- getBattle

getBattle :: Monad m => Battle m b b
getBattle = get

