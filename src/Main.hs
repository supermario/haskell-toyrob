{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding                 (Left, Right)
import Control.Lens                   (makeLenses, use, (%=), (?=), _Just)
import Control.Monad.Trans.Class      (lift)
import Control.Monad.Trans.State.Lazy (StateT, execStateT)
import Text.Read                      (readMaybe)

data Cardinal = North | East | South | West                                       deriving (Show, Enum, Read)
data Pose     = Pose (Integer, Integer, Cardinal)                                 deriving Show
data Board    = Board { width :: Integer, height:: Integer }                      deriving Show
data Robot    = Robot { _pose :: Pose }                                           deriving Show
data World    = World { _robot :: Maybe Robot, _board :: Board }                  deriving Show
data Command  = Place (Integer, Integer, Cardinal) | Move | Left | Right | Report deriving (Show, Read)

makeLenses ''Robot
makeLenses ''World

defaultWorld :: World
defaultWorld = World { _robot=Nothing, _board=Board { width=5, height=5 } }

main :: IO ()
main = run defaultWorld

run :: World -> IO ()
run oldWorld = do
  line <- getLine
  case readMaybe line :: Maybe Command of
    Just c  -> do
      newWorld <- execStateT (cmd c) oldWorld
      run newWorld
    Nothing -> do
      run oldWorld

cmd :: Command -> StateT World IO ()
cmd c = do
  b <- use board
  case c of
    Place (x,y,d) -> place (Pose(x,y,d)) b
    Move          -> mapRobot $ try move b
    Left          -> mapRobot $ try (rotate (-1)) b
    Right         -> mapRobot $ try (rotate   1 ) b
    Report        -> report

mapRobot :: (Robot -> Robot) -> StateT World IO ()
mapRobot f = robot._Just %= f

try :: (Pose -> Pose) -> Board -> Robot -> Robot
try f b r = Robot { _pose=result }
  where old    = _pose r
        new    = f old
        result = if isBounded new b then new else old

isBounded :: Pose -> Board -> Bool
isBounded (Pose(x,y,_)) b = x < width(b) && y < height(b) && x >= 0 && y >= 0

place :: Pose -> Board -> StateT World IO ()
place v b = case isBounded v b of
  True  -> robot ?= Robot { _pose=v }
  False -> return ()

report :: StateT World IO ()
report = do
  mr <- use robot
  lift . putStrLn $ s mr
  where s (Just r) = show $ _pose r
        s Nothing  = "Unplaced"

move :: Pose -> Pose
move (Pose (x,y,z)) = case z of
  North -> Pose (x,   y+1, North)
  East  -> Pose (x+1, y,   East)
  South -> Pose (x,   y-1, South)
  West  -> Pose (x-1, y,   West)

rotate :: Int -> Pose -> Pose
rotate d (Pose(x,y,z)) = Pose (x, y, toEnum(((fromEnum z)+d) `mod` 4))
