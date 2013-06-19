module Main where

import Control.Monad
import UI.NCurses --from the package 'ncurses'
import Cell

main :: IO ()
main = runCurses $ do
    win <- defaultWindow
    setEcho False
    setCursorMode CursorVisible
    worldLoop initialWorld
  
worldLoop :: World -> Curses ()
worldLoop w = do
  printWorld w
  win <- defaultWindow
  key <- getPressedKey win
  worldLoop $ case key of 
    EventSpecialKey KeyLeftArrow -> goLeft w
    EventSpecialKey KeyRightArrow -> goRight w
    EventSpecialKey KeyUpArrow -> goUp w
    EventSpecialKey KeyDownArrow -> goDown w
    EventCharacter ' ' -> setCell w $ not $ getCell w
    EventCharacter '\n' -> stepWorld w --Turns out EventSpecialKey KeyEnter ain't a thing.
    EventCharacter 'r' -> initialWorld
    _   -> w --in case something else filtered through
    
getPressedKey :: Window -> Curses Event
getPressedKey w = do
  ev <- getEvent w Nothing
  case ev of
    Nothing -> getPressedKey w
    Just ev' -> if keyFilter ev' then return ev' else getPressedKey w
  where
    keyFilter :: Event -> Bool
    keyFilter (EventCharacter _) = True
    keyFilter (EventSpecialKey _) = True
    keyFilter _ = False
    
drawChar :: Char -> Update ()
drawChar c = drawLineH (Just $ Glyph c []) 1 --for some reason this is not a default in ncurses
  
printWorld :: World -> Curses () --Print the world
printWorld world = do
  (rowsInteger, columnsInteger) <- screenSize
  let (rows, columns) = (fromIntegral rowsInteger, fromIntegral columnsInteger)
  
  let worldBox = getPrintableWorld world rows columns 
  
  win <- defaultWindow
  
  writeColor <- newColorID ColorWhite ColorBlack 1 --We're going to print spaces, so the background is what will be displayed
  eraseColor <- newColorID ColorWhite ColorWhite 2
  infoColor <- newColorID ColorWhite ColorRed 3
  
  updateWindow win $ do
    setAttribute AttributeBold True
    
    forM_ (zip [0..] worldBox) (\(row, line) -> do
      forM_ (zip [0..] line) (\(col, cell) -> do
        moveCursor row col
        setColor $ if cell then writeColor else eraseColor
        drawChar ' ' ) )
        
    moveCursor (rowsInteger - 1) 0
    setColor infoColor
    drawLineH (Just $ Glyph ' ' [AttributeDim]) columnsInteger --draw red line along the bottom
    drawString "Game of Life. Arrow keys move, space flips, enter evolves, R resets."
    
    setColor $ if getCell world then writeColor else eraseColor --color the middle cursor depending on whether or not a cell is present
    moveCursor (rowsInteger `div` 2) (columnsInteger `div` 2)
    drawLineH (Just $ Glyph 'X' []) 1 --This just draws one glyph in the middle
    
  render




