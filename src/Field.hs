{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}


module Field (
  Field(),
  enterPlayers,
  locatedAt,
  Coord(),
  (*#),
  v1,
  v2,
  v3,
  origo,
  chebDist,
  euclDist,
  innerProd,
  -- players,
  -- whoseTurn,
  -- history,
  -- pieces,
  -- position,
  -- health,
  -- actions,
  -- controller,
  -- roundNumber,
  updatePieceHealth,
  foldPieces,
  applyAction,
  updateHistory,
  ableToPlay,
  allianceAbleToPlay,
  getAlliances,
  stillContinues,
  winningPosition,
  getAvailableActions,
  updatePieceActions,
  swapPieces,
  addMorePlayer,
  addPieces,
  addInitialActions
             ) where

import Control.Lens
import Control.Monad
import qualified Data.Set as Set
import Data.Bifunctor as Bi

-----------------------------------
--- Coordinate system for field ---
-----------------------------------

newtype Coord = Coord (Double,Double,Double) deriving (Eq,Show,Ord)

instance Num Coord where
  Coord (a,b,c) + Coord (d,e,f) = Coord (a+d,b+e,c+f)
  Coord (a,b,c) - Coord (d,e,f) = Coord (a-d,b-e,c-f)
  Coord (a,b,c) * Coord (d,e,f) = Coord (a*d,b*e,c*f)
  abs (Coord (a,b,c)) = Coord (abs a, abs b, abs c)
  signum (Coord (a,b,c)) = case (compare a 0, compare b 0, compare c 0) of
    (GT, GT, GT) -> 1
    (LT, LT, LT) -> -1
    _ -> 0
  fromInteger x = Coord (fromInteger x, fromInteger x, fromInteger x)

(*#) :: Double -> Coord -> Coord
m *# (Coord (a,b,c)) = Coord (m*a, m*b, m*c)

origo = 0 *# v1

v1 :: Coord
v1 = Coord (1,0,0)

v2 :: Coord
v2 = Coord (0,1,0)

v3 :: Coord
v3 = Coord (0,0,1)

chebDist :: Coord -> Coord -> Double
chebDist (Coord (a,b,c)) (Coord (d,e,f)) = foldr max 0 [abs (a-d), abs (b-e), abs (c-f)]

euclDist :: Coord -> Coord -> Double
euclDist c d = sqrt $ innerProd c d

innerProd :: Coord -> Coord -> Double
innerProd (Coord (a,b,c)) (Coord (d,e,f)) = a*d + b*e + c*f

--------------------------------
---------- Field ---------------
--------------------------------

newtype Alliance = Alliance Int deriving (Eq,Ord,Show)

data Field n p a trn = Field
  {
    _players :: (n, n, [n])
  , _whoseTurn :: trn
  , _roundNumber :: Int
  , _history :: [(Int, a)]
  , _playerState :: n -> Player p
  , _pieceState :: p -> Piece n a
  , _winningPosition :: Field n p a n -> Bool
  } deriving (Functor)

data Piece n a = Piece
  {
    _position' :: Coord
  , _health' :: Double
  , _actions' :: [a]
  , _controller' :: n
  } deriving (Eq,Show,Functor)

data Player p = Player
  {
    _pieces' :: [p]
  , _alliance' :: Alliance
  } deriving (Eq,Show,Functor)

makeLensesFor [("_players", "players"),
               ("_whoseTurn", "whoseTurn"),
               ("_roundNumber", "roundNumber"),
               ("_history", "history")] ''Field

makeLenses ''Piece
makeLenses ''Player

instance Bifunctor (Field n p) where
  second = fmap
  first f fld =
    fld {_history = map (\(x,y) -> (x, f y)) . _history $ fld,
         _pieceState = \p -> fmap f (_pieceState fld p),
         _winningPosition = const False}

locatedAt :: (Eq p, Eq n) => Coord -> Lens' (Field n p a trn) [p]
locatedAt c = lens (findPieces c) (\x ps -> foldr (\p y -> (position p %~ const c) y) x ps)

playerList :: Field n p a trn -> [n]
playerList fld = fld ^. players & toList

pieceState :: (Eq p) => p -> Lens' (Field n p a trn) (Piece n a)
pieceState p =
  lens (\x -> _pieceState x p) (\x p' -> x {_pieceState = \m -> if p == m then p' else _pieceState x p})

playerState :: (Eq n) => n -> Lens' (Field n p a trn) (Player p)
playerState n =
  lens (\x -> _playerState x n) (\x n' -> x {_playerState = \m -> if n == m then n' else _playerState x n})

position p = pieceState p . position' 
health p = pieceState p . health' 
actions p = pieceState p . actions' 
controller p = pieceState p . controller' 

pieces n = playerState n . pieces'
alliance n = playerState n . alliance'

----

toList :: (a,a,[a]) -> [a]
toList (a,b,as) = a:b:as

----

findPieces :: (Eq n, Eq p) => Coord -> Field n p a trn -> [p]
findPieces c f = do
  pc <- allPieces f
  let (a,b) = (f ^. position pc,pc)
  guard $ a == c
  return b

findMembers :: (Eq n) => Alliance -> Field n p a trn -> [n]
findMembers a f = do
  pl <- playerList f
  let as = f ^. alliance pl
  guard $ as == a
  return pl

allPieces :: (Eq n, Eq p) => Field n p a trn -> [p]
allPieces f = do
  pl <- f ^. players & toList
  pc <- f ^. pieces pl
  return pc
    

foldrOverPlayers f fld = foldr f fld (fld ^. players & toList)

foldrOverPieces :: (Eq n, Eq p) => (p -> Field n p a trn -> Field n p a trn) -> Field n p a trn -> Field n p a trn
foldrOverPieces f fld = foldr f fld (allPieces fld)

transformPlayer :: Player () -> Player p
transformPlayer (Player _ a) = Player [] a

piecefulField :: (n -> [p]) -> (p -> n) -> Field n () () trn -> Field n p () trn
piecefulField pc ns fldd = Field
  {_pieceState = \p -> Piece origo 0 [] (ns p),
    _playerState = \n -> (transformPlayer . flip _playerState n $ fldd) {_pieces' = pc n},
    _players = _players fldd,
    _whoseTurn = _whoseTurn fldd,
    _roundNumber = _roundNumber fldd,
    _history = _history fldd,
    _winningPosition = const False
  }


---------------------------
---------- API ------------
---------------------------

enterPlayers :: Eq n => n -> n -> Field n () () n
enterPlayers p1 p2 =
  Field (p1,p2,[]) p1 0 [] f (\_ -> Piece origo 0 [] p1) (const False)
  where
    f = \p -> Player [] (if p == p1 then Alliance 1 else Alliance 2)

addMorePlayer :: Eq n => n -> Alliance -> Field n () () trn -> Field n () () trn
addMorePlayer p a fld = (players %~ (\(x,y,z) -> (x,y,p:z))) . (alliance p %~ (const a)) $ fld

addPieces :: Eq n => (n -> [p]) -> (p -> n) -> Field n () () trn -> Field n p () trn
addPieces pc ns fld = foldrOverPlayers (\n -> pieces n %~ (const (pc n))) . piecefulField pc ns $ fld

addInitialActions :: (Monoid a, Eq n, Eq p) => (p -> [a]) -> Field n p () n -> Field n p a n
addInitialActions pf = foldrOverPieces (\p -> actions p %~ const (pf p)) . Bi.first (\_ -> mempty)

getAvailableActions :: (Eq p, Eq n, Eq a, Monoid a) => Field n p a n -> [a]
getAvailableActions fld = map (foldMap id) . sequenceA $ do
  let cp = fld ^. whoseTurn
  pc <- fld ^. pieces cp
  return . f $ fld ^. actions pc
  where
    f x = if mempty `elem` x then x else mempty : x
  

updatePieceHealth :: Eq p => (Coord -> Coord) -> (Double -> Double) -> p -> Field n p a trn -> Field n p a trn
updatePieceHealth coordTf healthTf p fld =
  let
    newHealth = (health p %~ healthTf) fld
  in
    case newHealth ^. health p of
      0 -> (position p %~ coordTf) newHealth
      _ -> newHealth

updatePieceActions :: (Eq p, Eq a) => ([a] -> [a]) -> p -> Field n p a trn -> Field n p a trn
updatePieceActions actUpd p fld = (actions p %~ actUpd) fld

swapPieces :: Eq p => p -> p -> Field n p a trn -> Field n p a trn
swapPieces p1 p2 fld =
  (position p1 %~ const p2c) . (position p2 %~ const p1c) $ fld
  where
    p1c = fld ^. position p1
    p2c = fld ^. position p2
  

updateHistory :: (Int -> Int) -> (trn -> trn') -> a -> Field n p a trn -> Field n p a trn'
updateHistory roundIncr movingPlayerUpdate action fld =
  fmap movingPlayerUpdate .
  (roundNumber %~ roundIncr) .
  (history %~ ((:) (fld ^. roundNumber, action))) $ fld
  
foldPieces, applyAction :: Eq n => (a -> p -> p) -> a -> Field n p a trn -> Field n p a trn
foldPieces f action fld = foldrOverPlayers (\m -> foldOverPlayer m f action) fld 
applyAction = foldPieces

foldOverPlayer :: Eq n => n -> (a -> p -> p) -> a -> Field n p a trn -> Field n p a trn
foldOverPlayer n f action fld = (pieces n %~ map (f action)) fld

ableToPlay :: (Eq n, Eq p) => Field n p a trn -> n -> Bool
ableToPlay fld n = foldr (||) False $ do
  x <- fld ^. pieces n
  let y = fld ^. health x
  return $ y > 0

allianceAbleToPlay :: (Eq n, Eq p) => Field n p a trn -> Alliance -> Bool
allianceAbleToPlay fld a = foldr (||) False . map (ableToPlay fld) $ findMembers a fld

getAlliances :: (Eq n) => Field n p a trn -> [Alliance]
getAlliances fld = Set.toList . Set.fromList . map (\x -> fld ^. alliance x) . playerList $ fld

stillContinues :: (Eq n, Eq p) => Field n p a trn -> Bool
stillContinues fld = length x <= 1
  where
    x = filter id . map (allianceAbleToPlay fld) . getAlliances $ fld

winningPosition :: Field n p a n -> Bool
winningPosition fld = _winningPosition fld fld
  
-- noLegalMoves :: Field n p a trn -> Bool
-- noLegalMoves fld =
  -- let
    -- plrs = fld ^. players
    -- alive = length . filter id . (\x -> ableToPlay x fld) . toList $ plrs
  -- in undefined
