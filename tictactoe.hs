module Main where

import Data.Char
import System.Random

type Row = [String]
type Col = [String]
type Board = [Row]

board :: Board
board = [["-", "-", "-"]] ++ [["-", "-", "-"]] ++ [["-", "-", "-"]]

printRowRow :: Row -> String
printRowRow row = (unwords row) ++ "\n"

printRow :: Board -> Int -> String
printRow b i = unwords (b !! i) ++ "\n"

board2String :: Board -> Int -> String -> String
board2String b i str = if i < (length b) then str ++ (board2String b (i + 1) (printRow b i)) else str

printBoard :: Board -> IO ()
printBoard b = putStrLn("\n" ++ board2String b 0 "\n")

boardLength = read (show (length board)) :: Float

isStringANumber :: String -> Bool
isStringANumber (x:xs)
  | l == 0 = if isDigit x then True else False
  | isDigit x = isStringANumber xs
  | otherwise = False
  where
    l = length xs

playCell :: Int -> Int -> String -> String -> String
playCell index x el csub
  | el == "O" = el
  | el == "X" = el
  | index == x = csub
  | otherwise = el
  
playRow :: Int -> Row -> String -> Row
playRow x row csub = zipWith (\index el -> playCell index x el csub) [0..] row

playBoard :: Int -> Int -> Board -> String -> Board
playBoard c r b csub = zipWith (\index el -> if index == r then playRow c row csub else el) [0..] b
   where
     row = b !! r

isMyTurn :: Int -> Bool
isMyTurn n
  | m == 0 = True
  | otherwise = False
  where
    m = n `mod` 2

getNextInput :: String -> String
getNextInput input
  | isStringANumber input = ""
  | otherwise = "\n" ++ input

getCSub :: Bool -> String
getCSub myTurn 
  | myTurn == True = "X"
  | otherwise = "O"

findWinnerRow :: Row -> String -> Bool
findWinnerRow row csub = foldr (&&) True truthList
  where
    truthList = map (\c -> c == csub) row

getColumn :: Board -> Int -> Row
getColumn board colIndex = map (\row -> row !! colIndex) board

findWinnerCol :: Board -> Int ->  String -> Bool
findWinnerCol board colIndex csub = foldr (&&) True truthList
  where
    col = getColumn board colIndex
    truthList = map (\c -> c == csub) col

findWinnerRowHorizontal :: Board -> String -> Bool
findWinnerRowHorizontal board csub = foldr (||) False truthList
  where
    truthList = map (\row -> findWinnerRow row csub) board

findWinnerRowVertical :: Board -> String -> Int -> [Bool] -> Bool
findWinnerRowVertical board csub colIndex acc
  | colIndex >= (rowLength - 1) = foldr (||) False truthList
  -- | colIndex >= (rowLength - 1) = truthList
  | otherwise = findWinnerRowVertical board csub (colIndex + 1) truthList
  where
    winningCol = findWinnerCol board colIndex csub
    truthList = acc ++ [winningCol]
    rowLength = length (board !! 0)

-- 0 4 8
-- 2 4 6
-- findWinnerDiagonal :: Board -> String -> Bool
-- findWinnerRowHorizontal board csub = foldr (||) False truthList
--   where
--     cBoard = concat board
--     truthList = map (\row -> findWinnerRow row csub) board

findWinner :: Board -> String -> Bool
findWinner board csub = (findWinnerRowHorizontal board csub) || (findWinnerRowVertical board csub 0 [])

getPlayablePositionsRow :: Row -> [Int]
getPlayablePositionsRow row = filter (>= 0) zipped
  where
    zipped = zipWith (\index c -> if c == "-" then index else -1) [0..] row

getPlayablePositions :: Board -> [Int]
getPlayablePositions [] = []
getPlayablePositions board = getPlayablePositionsRow (concat board)

seed::Int
seed = 40

generator = mkStdGen seed

getPlayablePosition :: [Int] -> Int
getPlayablePosition giveList = giveList !! rand where
  n = length giveList
  (rand, _) = randomR (0,(n-1)) generator

prompt :: Board -> String -> Int -> IO ()
prompt board prevInput count
  | playablePositionsCount == 0 = do 
    printBoard board
    putStrLn("Patta !!!")
  | youWon == True = do 
    printBoard board
    putStrLn("Congraturations ! You Won !!!")
  | pcWon == True = do 
    printBoard board
    putStrLn("Ops :( - PC Won !!!")
  | prevInput == "q" = do putStrLn("Exit")
  | myTurn == False = do
    putStrLn(prevInput)
    
    printBoard board
    
    putStr("\nPC Playing: ")
    
    let rnd = getPlayablePosition (getPlayablePositions board)

    let _pos = read (show rnd) :: Float
    let row = floor (_pos / boardLength)

    let pos = mod (floor _pos) (length board)

    let newBoard = playBoard pos row board csub

    putStrLn("Is My Turn: " ++ (show myTurn))

    putStrLn("PC choose c" ++  (show pos) ++ ",r" ++ (show row))

    prompt newBoard (getNextInput (show rnd)) (count + 1)
  | otherwise = do
      putStrLn(prevInput)

      putStrLn "You Won ?"
      print (findWinner board csub)
      
      printBoard board

      putStr "\nDo your move: "
      
      input <- getLine
      
      let _pos = read input :: Float
      let row = floor (_pos / boardLength)

      let pos = mod (floor _pos) (length board)

      let newBoard = playBoard pos row board csub
      
      putStrLn("Is My Turn: " ++ (show myTurn))

      putStrLn("You choose c" ++  (show pos) ++ ",r" ++ (show row))
    
      prompt newBoard (getNextInput input) (count + 1)
    where
      myTurn = isMyTurn count
      csub = (getCSub myTurn)
      -- csub = "X"
      playablePositions = (getPlayablePositions board)
      playablePositionsCount = length playablePositions
      gameIsWon = findWinner board csub
      youWon = gameIsWon && myTurn
      pcWon = gameIsWon && (myTurn == False)

main = prompt board "Welcome to Tic Tac Toe" 0
