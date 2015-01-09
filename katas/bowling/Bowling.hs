module Bowling where

getScore :: [Int] -> Int
getScore [] = 0
getScore scores = (getFrameScore frame rest) + getScore rest
        where
          numberOfThrows = nextFrameThrows scores
          frame = take numberOfThrows scores
          rest = drop numberOfThrows scores

nextFrameThrows :: [Int] -> Int
nextFrameThrows (x:_)
  | x == numberOfPins = 1
  | otherwise = 2

numberOfPins :: Int
numberOfPins = 10

getFrameScore :: [Int] -> [Int] -> Int
getFrameScore frame rest
  | isSpare frame && hasMoreFrames rest
    = sum frame + sum (take (3 - length frame) rest)
  | otherwise = sum frame

hasMoreFrames :: [Int] -> Bool
hasMoreFrames rest = length rest > 2

isSpare :: [Int] -> Bool
isSpare [] = False
isSpare (frame) = (sum frame) == numberOfPins
