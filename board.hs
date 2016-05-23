-- Last Modified: 11/25/15

module Board where

  -- Function to create the board given an int variable that will determine the size
  mkBoard :: Int -> [String]
  mkBoard x = mkBoardList (x*x)

  -- Function to create the list of strings for the board based on the size of the board
  mkBoardList :: Int -> [String]
  mkBoardList x
    | x <= 0 = []
    | otherwise = place:mkBoardList (x-1)
    where place = "-"

  -- Function that will mark a hit on the board given a column and row
  hitBoard:: Int -> Int -> [String] -> [String]
  hitBoard x y board = do
    if(hasShip x y board) == True then markPlace (calcCoord x y board) "X" board else markPlace (calcCoord x y board) " " board

  -- Function that will place a ship on the board given a column, row, direction, and size of ship
  placeShip :: Int -> Int -> Int -> Bool -> [String] -> [String]
  placeShip n x y dir board
    | isShipPlaceable n x y dir board == False = board
    | otherwise = do
      if dir == False then
        placeVerticalShip n x y board
      else placeHorizontalShip n x y board

  -- Function that will place a vertical ship on the board
  placeVerticalShip:: Int -> Int -> Int -> [String] -> [String]
  placeVerticalShip n x y board
    | n <= 0 = board
    | otherwise =
      let c = calcCoord x y board in
        placeVerticalShip (n-1) x (y+1) (placeCoordinate c board)

  -- Function that will place a horizontal ship on the board
  placeHorizontalShip:: Int -> Int -> Int -> [String] -> [String]
  placeHorizontalShip n x y board
    | n <= 0 = board
    | otherwise =
      let c = calcCoord x y board in
        placeHorizontalShip (n-1) (x+1) y (placeCoordinate c board)

  -- Function that will calculate the coordinate for a single list given a row and column
  calcCoord:: Int -> Int -> [String] -> Int
  calcCoord x y board = (boardLength * (y - 1) + x) - 1
    where boardLength = getBoardLength board

  -- Function that will place a ship coordinate on the board as a "O"
  placeCoordinate:: Int -> [String] -> [String]
  placeCoordinate c board = markPlace c "O" board

  -- Function that will return true or false if the given coordinate is hit
  isHit:: Int -> Int -> [String] -> Bool
  isHit x y board = (board !! calcCoord x y board) == "X"

  -- Function that will return true or false if the given coordinate is hit
  hasShip:: Int -> Int -> [String] -> Bool
  hasShip x y board = (board !! calcCoord x y board) == "O"

  -- Function that will mark the places of a ship on the board
  markPlace:: Int -> String -> [String] -> [String]
  markPlace n marker (x: xs)
    | n == 0 = marker:xs
    | otherwise = x:markPlace (n-1) marker xs

  -- Function that returns true or false if all ships have been sunk
  isGameOver:: [String] -> Bool
  isGameOver (x:xs)
    | xs == [] = True
    | x == "O" = False
    | otherwise = isGameOver xs

  -- Function that will convert the board lists to string
  boardToStr:: (String -> String) -> [String] -> [[String]]
  boardToStr marker board = printBoard marker board (getBoardLength board)

  -- Function that will print the board as multiple lists (2D array) since it is a single list
  printBoard:: (String -> String) -> [String] -> Int -> [[String]]
  printBoard marker board n
    | board == [] = []
    | otherwise = [printPlaces marker line] ++ printBoard marker (drop n board) n
    where line = take n board

  -- Function used by printBoard to print the board as multiple lists (2D array)
  printPlaces:: (String -> String) -> [String] -> [String]
  printPlaces marker (x:xs)
    | xs == [] = []
    | otherwise = [marker x] ++ printPlaces marker xs

  -- Function that will print a single place of the board without cheating
  sqrToStr:: String -> String
  sqrToStr n
    | n == "O" = "-"
    | otherwise = n

  -- Function that will print a single place of the board with cheating
  sqrToStrCheat::String -> String
  sqrToStrCheat n = n

  -- Function that will calculate the board length based on the given size
  getBoardLength:: [String] -> Int
  getBoardLength board = intSquareRoot(length board)

  -- Function used to get board length
  intSquareRoot:: Int -> Int
  intSquareRoot n = aux n
    where
      aux x
        | x * x > n = aux(x-1)
        | otherwise = x

  -- Function to check if board is placeable on the board based on input and size of ship
  isShipPlaceable:: Int -> Int -> Int -> Bool -> [String] -> Bool
  isShipPlaceable n x y dir board
    | n == 0 = False
    | x <= 0 || x > boardLength = False
    | y <= 0 || y > boardLength = False
    | otherwise = do
      if dir == True then
        checkHorizontalShip n x y board
      else
        checkVerticalShip n x y board
    where boardLength = getBoardLength board

  -- Function that will check if the vertical ship is placeable
  checkVerticalShip:: Int -> Int -> Int -> [String] -> Bool
  checkVerticalShip n x y board
    | n == 0 = True
    | ((y-1) + n) > boardLength = False
    | otherwise = do
      if board !! c == "O" then False
      else checkVerticalShip (n-1) x (y+1) board
    where boardLength = getBoardLength board
          c = calcCoord x y board

  -- Function that will check if a horizontal ship is placeable
  checkHorizontalShip:: Int -> Int -> Int -> [String] -> Bool
  checkHorizontalShip n x y board
    | n == 0 = True
    | ((x-1) + n) > boardLength = False
    | otherwise = do
      if board !! c == "O" then False
      else checkHorizontalShip (n-1) (x+1) y board
    where boardLength = getBoardLength board
          c = calcCoord x y board
