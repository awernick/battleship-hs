-- Last Modified: 11/25/15

import Board
import System.Random
import System.IO.Unsafe

-- Function that randomizes an int given the range from a to b
getRandom :: Int -> Int -> Int
getRandom a b = unsafePerformIO $ randomRIO(a,b)

-- Function that places all ships given a list of the sizes of ships
placeShips:: [Int] -> [String] -> [String]
placeShips ships@(h:hs) board
  | hs == [] = board
  | otherwise = do
      if isShipPlaceable h x y dir board == True then
        if dir == True then
          placeShips hs (placeHorizontalShip h x y board)
        else
          placeShips hs (placeVerticalShip h x y board)
      else
        placeShips ships board
  where dir = if getRandom 1 10 > 5 then True else False
        x = getRandom 1 10
        y = getRandom 1 10

-- Function that gets the column and row for the shot
getXY board = do
  putStrLn "Enter column to make shot"
  column <- getLine
  putStrLn "Enter row to make shot"
  row <- getLine
  return (read(column)::Int, read(row)::Int)

-- Ships list and main board for the game
ships = [5,4,3,2,2,2]
mainBoard = placeShips ships (mkBoard 10)

-- Function that allows you to play the game until game over
play marker board = do
  (x, y) <- getXY board
  let shotBoard = hitBoard x y board in do
    if isGameOver shotBoard then
      print "Game Over! You won!"
    else do
      mapM_ print (boardToStr marker shotBoard)
      play marker shotBoard

-- Function used to play the game with a regular board
main :: IO ()
main = do
  mapM_ print (boardToStr sqrToStr mainBoard)
  play sqrToStr mainBoard

-- Function used to play the game with a cheating board
mainCheat :: IO ()
mainCheat = do
  mapM_ print (boardToStr sqrToStrCheat mainBoard)
  play  sqrToStrCheat mainBoard
