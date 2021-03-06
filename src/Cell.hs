module Cell 
(
  Cell,
  World,
  getCell,
  setCell,
  goRight,
  goLeft,
  goUp,
  goDown,
  goX,
  goY,
  stepWorld,
  initialWorld,
  getPrintableWorld
) where

import qualified Data.Set as S

type Cell = Bool 
type Position = (Int, Int)
data World = World Position (S.Set Position) --A world is a position (x, y) and a set of live cell positions.

--Cell functions
getCell :: World -> Cell --Get the cell we're focused on
getCell (World pos set) = S.member pos set

setCell :: World -> Cell -> World
setCell (World pos set) True = World pos (S.insert pos set)
setCell (World pos set) False = World pos (S.delete pos set)

goRight :: World -> World --Move the focus right
goRight (World (x, y) set) = World (x+1, y) set

goLeft :: World -> World
goLeft (World (x, y) set) = World (x-1, y) set

goUp :: World -> World
goUp (World (x, y) set) = World (x, y+1) set

goDown :: World -> World
goDown (World (x, y) set) = World (x, y-1) set

goX :: World -> Int -> World --Move in the X direction n spaces
goX (World (x, y) set) n = World (x+n, y) set

goY :: World -> Int -> World --Move in the Y direction n spaces
goY (World (x, y) set) n = World (x, y+n) set

generateNeighbours :: Position -> [Position]
generateNeighbours (x, y) = [(x-1,y+1), (x,y+1), (x+1,y+1),
                             (x-1,y)  ,          (x+1,y),
                             (x-1,y-1), (x,y-1), (x+1,y-1)]
                          
stepWorld :: World -> World
stepWorld (World pos set) = World pos set'
  where
    set' = S.fold evolvePoint S.empty set
    evolvePoint pos accSet = accSet `S.union` S.fromList newLives
      where
        newLives = filter evolveCondition $ pos : generateNeighbours pos
        evolveCondition (x, y) = evolve (S.member (x, y) set) $ liveNeighbours (x, y)
        liveNeighbours pos = length . filter (flip S.member set) $ generateNeighbours pos
        evolve :: Cell -> Int -> Cell --Basic behavior rules of Conway's Game of Life
        evolve _ 3 = True
        evolve True 2 = True
        evolve _ _ = False
    
initialWorld :: World
initialWorld = World (0, 0) S.empty
  
getPrintableWorld :: World -> Int -> Int -> [[Bool]]
getPrintableWorld world rows columns = result
  where
    centeredWorld = goY (goX world $ negate $ columns `div` 2) (rows `div` 2) --Move the world down and to the right, 'centering' it for printing
    convertRow = map getCell . take columns . iterate goRight
    result = map convertRow $ take rows $ iterate goDown centeredWorld

