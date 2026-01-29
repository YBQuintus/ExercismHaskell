module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot
  { robotBearing  :: Bearing
  , position :: (Integer, Integer)
  }

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ position) = position

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = (Robot direction coordinates)

turnRight :: Robot -> Robot
turnRight (Robot North position) = (Robot East position)
turnRight (Robot East position) = (Robot South position)
turnRight (Robot South position) = (Robot West position)
turnRight (Robot West position) = (Robot North position)

turnLeft :: Robot -> Robot
turnLeft r = turnRight $ turnRight $ turnRight r

moveOnce :: Robot -> Robot
moveOnce (Robot North (x, y)) = (Robot North (x, y+1))
moveOnce (Robot East (x, y)) = (Robot East (x+1, y))
moveOnce (Robot South (x, y)) = (Robot South (x, y-1))
moveOnce (Robot West (x,y)) = (Robot West (x-1, y))

move :: Robot -> String -> Robot
move robot "" = robot
move robot ('R':xs) = move (turnRight robot) xs
move robot ('L':xs) = move (turnLeft robot) xs
move robot (x:xs) = move (moveOnce robot) xs
