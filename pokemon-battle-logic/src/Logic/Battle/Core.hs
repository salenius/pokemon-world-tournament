module Logic.Battle.Core where

import Control.Monad.State
import Control.Monad.Reader
import Data.List
-- import Logic.Helper.Cont
import Control.Monad.Cont

data Team = Team1 | Team2 deriving (Eq,Show,Ord,Enum,Bounded)

type Battle m b = ContT Result (StateT b m) 
  
type OrderDeterminant plr = plr -> OrderNum

data OrderNum = OrderNum
  {
    specialPriority :: Int,
    movePriority :: Int,
    speedPriority :: Int
  } deriving (Eq,Show,Ord)

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

--------------------------------

playBattle :: (Monad m, Ord ch) => BattleOps m b ch plr -> Battle m b ()
playBattle fns = callCC $ \finish -> do
  choices <- makeChoices fns
  order <- determineOrder fns choices
  let sorted = sortChoices order choices
  executeChoices fns sorted [] finish
  endTurn fns finish
  playBattle fns

sortChoices :: Ord ch => OrderDeterminant plr -> [ch] -> [ch]
sortChoices = undefined

executeChoices ::
  (Monad m, Ord ch)
  => BattleOps m b ch plr
  -> [ch]
  -> [ExecData plr]
  -> Finisher m b
  -> Battle m b ()
executeChoices _ [] _ _ = return ()
executeChoices fns (ch:chs) hist finish = callCC $ \skip -> do
  let terminate x = case x of
        ToEndOfTurn -> skip
        ToNextExec  -> finish
  x <- executeChoice fns ch hist terminate
  y <- updateChoices fns x chs
  executeChoices fns y (x:hist) finish

data Result = WinningTeam Team deriving (Eq,Show,Ord)

data BattleOps m b ch plr = BattleOps
  {
    makeChoices :: ChoiceMaker m b ch
  , determineOrder :: OrderMaker m b ch plr
  , executeChoice :: Executioner m b ch plr
  , updateChoices :: ExecData plr -> [ch] -> Battle m b [ch]
  , endTurn :: Finisher m b -> Battle m b ()
  }

data ExecEnding = ToEndOfTurn | ToNextExec deriving (Eq,Show,Ord,Enum)

type Finisher m b = () -> Battle m b ()
type ChoiceMaker m b ch = Battle m b [ch]
type OrderMaker m b ch plr = [ch] -> Battle m b (OrderDeterminant plr)
type Executioner m b ch plr =
  ch -> [ExecData plr] -> (ExecEnding -> Finisher m b) -> Battle m b (ExecData plr)
  
-------

getBattle :: Monad m => Battle m b b
getBattle = get

---

-- startTurn StartTurn = makeChoices >>= determineFaster >>= firstExecution

-- firstExecution (i,i') = executeMoves (i,i',[])

-- executeMoves input = determineUser input >>= execUntil executeMoves wrapUpTurn

-- wrapUpTurn = endTurn >>= startTurn


----
