module Cell 
(
  Cell,
  Column,
  World,
  getCell,
  setCell,
  goRight,
  goLeft,
  goUp,
  goDown,
  goX,
  goY,
  stepColumn,
  stepWorld,
  initialWorld,
  getPrintableWorld
) where

import qualified ZipperList as ZL

type Cell = Bool
type Column = ZL.ZipperList Cell
type World = ZL.ZipperList Column

--Cell functions
getCell :: World -> Cell --Get the cell we're focused on
getCell = ZL.getFocus . ZL.getFocus

setCell :: World -> Cell -> World
setCell w c = ZL.setFocus w $ ZL.setFocus (ZL.getFocus w) c

goRight :: World -> World --Move the focus right
goRight = ZL.goForward

goLeft :: World -> World
goLeft = ZL.goBack

goUp :: World -> World
goUp (cols, prevCols) = (map ZL.goForward cols, map ZL.goForward prevCols)

goDown :: World -> World
goDown (cols, prevCols) = (map ZL.goBack cols, map ZL.goBack prevCols)

goX :: World -> Int -> World --Move in the X direction n spaces
goX w n = iterate (if n >= 0 then goRight else goLeft) w !! (abs n)

goY :: World -> Int -> World --Move in the Y direction n spaces
goY w n = iterate (if n >= 0 then goUp else goDown) w !! (abs n)

stepColumn :: Column -> Column -> Column -> Column --Specifically, takes the column to the left, the column to be stepped, and the column to the right
stepColumn lCol stepCol rCol = (stepUp lCol stepCol rCol, stepDown (ZL.goBack lCol) (ZL.goBack stepCol) (ZL.goBack rCol))
  where 
    stepUp :: Column -> Column -> Column -> [Cell]
    stepUp lCol col@(c:_,_) rCol = evolve c (neighbours lCol col rCol):stepUp (ZL.goForward lCol) (ZL.goForward col) (ZL.goForward rCol)
    
    stepDown :: Column -> Column -> Column -> [Cell]
    stepDown lCol col@(c:_,_) rCol = evolve c (neighbours lCol col rCol):stepDown (ZL.goBack lCol) (ZL.goBack col) (ZL.goBack rCol)
    
    evolve :: Cell -> Int -> Cell --Basic behavior rules of Conway's Game of Life
    evolve _ 3 = True
    evolve True 2 = True
    evolve _ _ = False
    
    neighbours :: Column -> Column -> Column -> Int --Number of neighbours surrounded by the focus in middle column
    neighbours (l:tl:_, bl:_) (_:t:_, b:_) (r:tr:_, br:_) = length $ filter id $ [tl,t,tr,l,r,bl,b,br]

stepWorld :: World -> World
stepWorld w = (stepRight w, stepLeft (goLeft w))
  where
    stepRight :: World -> [Column]
    stepRight w@(col:rCol:_, lCol:_) = stepColumn lCol col rCol:stepRight (goRight w)
    
    stepLeft :: World -> [Column]
    stepLeft w@(col:rCol:_, lCol:_) = stepColumn lCol col rCol:stepLeft (goLeft w)
    
initialWorld = (repeat blankColumn, repeat blankColumn) --Infinite worlds for all! 
  where blankColumn = (repeat False, repeat False)
  
getPrintableWorld :: World -> Int -> Int -> [[Bool]]
getPrintableWorld world rows columns = result
  where
    centeredWorld = goY (goX world $ negate $ columns `div` 2) (rows `div` 2) --Move the world down and to the right, 'centering' it for printing
    convertRow = map getCell . take columns . iterate goRight
    result = map convertRow $ take rows $ iterate goDown centeredWorld

