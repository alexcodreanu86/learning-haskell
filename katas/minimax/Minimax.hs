module Minimax where

import Data.List
import Data.Function

winningCombinations :: [[Int]]
winningCombinations = [
      [0,1,2]
    , [3,4,5]
    , [6,7,8]
    , [0,3,6]
    , [1,4,7]
    , [2,5,8]
    , [0,4,8]
    , [2,4,6]
  ]

cellNumbers :: [Int]
cellNumbers = [0..8]

getMove :: [(Int,Char)] -> Char -> Char -> Int
getMove board currentPlayer aiPlayer
  | currentPlayer == aiPlayer = getHighestScored movesWithScore
  | otherwise = getLowestScored movesWithScore
  where
        movesWithScore = getNextRoundMovesWithScores board currentPlayer aiPlayer

getNextRoundMovesWithScores :: [(Int,Char)] -> Char -> Char -> [(Int,Int)]
getNextRoundMovesWithScores board currentPlayer aiPlayer =
  let possibleMoves = getAvailableMoves board
      scorePerMove = map (\x -> scoreBoard ((x, currentPlayer):board)
                                             currentPlayer
                                             aiPlayer 0) possibleMoves
  in zip possibleMoves scorePerMove

otherPlayer :: Char -> Char
otherPlayer 'X' = 'O'
otherPlayer _ = 'X'

scoreBoard :: [(Int, Char)] -> Char -> Char -> Int -> Int
scoreBoard board currentPlayer ai depth
  | winner == Just ai = 10 - depth
  | winner == Just (otherPlayer ai) = depth - 10
  | isFull board = 0
  | otherwise = getNextRoundScore board currentPlayer ai (depth + 1)
    where winner = getWinner board

getNextRoundScore :: [(Int, Char)] -> Char -> Char -> Int -> Int
getNextRoundScore board currentPlayer ai depth =
  let
  nextPlayer = otherPlayer currentPlayer
  move = getMove board nextPlayer ai
   in scoreBoard ((move,nextPlayer):board) nextPlayer ai depth

isFull :: [(Int, Char)] -> Bool
isFull board = length board == 9

getHighestScored :: [(Int,Int)] -> Int
getHighestScored movesWithScores =
   fst (maximumBy (compare `on` snd) movesWithScores)

getLowestScored :: [(Int,Int)] -> Int
getLowestScored movesWithScores =
   fst (minimumBy (compare `on` snd) movesWithScores)

getAvailableMoves :: [(Int, Char)] -> [Int]
getAvailableMoves board =
  filter (`notElem` takenMoves) cellNumbers
  where takenMoves = map fst board

getWinner :: [(Int,Char)] -> Maybe Char
getWinner board =
  getPlayerWithMove board winningCombo
    where winningCombo = getWinningCombo board winningCombinations

getWinningCombo :: [(Int,Char)] -> [[Int]] -> Maybe [Int]
getWinningCombo [] _ = Nothing
getWinningCombo _ [] = Nothing
getWinningCombo board (combo:rest)
  | isWinningCombo board combo = Just combo
  | otherwise = getWinningCombo board rest

isWinningCombo :: [(Int, Char)] -> [Int] -> Bool
isWinningCombo board combo =
  let cells = getCellsWithNumbers board combo
      cellsSymbols = map snd cells
   in length cells == 3 && sameElements cellsSymbols

getCellsWithNumbers :: [(Int, Char)] -> [Int] -> [(Int, Char)]
getCellsWithNumbers board =
  foldl concatenateCells []
    where concatenateCells acc cell = acc ++ getCellsWithNumber board cell

sameElements :: String -> Bool
sameElements (x:y:z:_) = x == y && x == z
sameElements _ = False

getPlayerWithMove :: [(Int, Char)] -> Maybe [Int] -> Maybe Char
getPlayerWithMove _ Nothing = Nothing
getPlayerWithMove board (Just moves) = Just $ getSymbolAtPosition board (head moves)

getSymbolAtPosition :: [(Int,Char)] -> Int -> Char
getSymbolAtPosition board cellNumber = snd $ head $ getCellsWithNumber board cellNumber

getCellsWithNumber :: [(Int, Char)] -> Int -> [(Int, Char)]
getCellsWithNumber board cellNumber = [x | x <- board, fst x == cellNumber ]
