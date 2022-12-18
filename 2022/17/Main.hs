{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import      qualified     Data.HashSet         as Hashset
import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import qualified Data.List as List
import Data.HashSet (fromList, empty, member, intersection, union, toList)
import Data.Tuple (swap)


main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = cycle $ parse content

    let total = 1000000000000
    let (n, rest) = total `divMod` patternSize
    let safeNumberOfPatterns = 5 -- 100 for test data

    let startOffset = rest + safeNumberOfPatterns * patternSize

    let part1Gen = take startOffset $ cycle [shape1, shape2, shape3, shape4, shape5]
    let ys = moveWithJet empty part1Gen (2, 3) input
    -- let part1 = higherY part1Rocks
    -- putStrLn $ draw part1Rocks [0..(30 * 7 - 1)]
    -- print (part1 + 1)


    let patternSum = sum ys - sum (take (startOffset - patternSize) ys)

    let endSum = sum ys + ((n - safeNumberOfPatterns) * patternSum) + 1


    -- print n
    -- print rest
    -- print patternSum
    print endSum
    -- print ys

    hClose handle

-- patternSize = length [1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2]
patternSize = length [1,0,1,3,3,4,2,1,3,3,0,0,0,2,2,2,0,1,2,3,0,2,1,3,3,2,0,1,2,1,2,0,1,3,2,4,2,1,3,3,2,2,1,2,2,2,2,0,0,3,2,2,1,2,1,3,0,1,2,3,2,0,1,3,0,3,0,1,3,3,4,2,1,3,3,2,0,1,2,1,3,0,0,1,3,0,0,1,3,3,2,0,1,3,3,4,0,1,3,2,2,0,1,2,1,2,0,1,3,3,2,0,1,3,0,3,2,1,2,3,0,2,0,2,2,0,0,1,2,2,4,0,1,3,3,0,2,1,3,3,4,0,0,2,0,0,0,1,3,2,0,0,1,3,2,2,2,1,3,3,0,0,0,0,2,2,0,1,3,2,1,2,1,2,2,2,2,1,3,2,2,0,1,3,2,4,2,1,2,1,2,2,1,3,2,2,0,1,0,3,2,0,1,2,2,2,0,1,0,2,2,0,1,3,2,2,2,1,2,1,2,2,0,0,3,1,0,0,3,2,2,0,1,3,3,4,2,1,3,0,2,0,1,3,3,2,0,0,2,1,0,0,1,2,1,2,2,0,3,3,2,2,1,3,3,2,2,1,0,0,2,0,1,3,3,4,0,1,3,2,2,2,1,3,3,0,0,1,3,3,2,0,1,3,0,1,0,1,2,3,2,0,1,3,2,4,2,1,3,0,2,0,0,2,1,3,0,1,2,3,2,0,1,3,0,2,0,1,2,3,0,2,1,3,2,4,0,1,2,3,4,2,1,3,3,4,0,1,3,3,2,0,1,3,0,2,0,1,3,3,2,2,1,2,2,2,2,1,3,3,0,0,1,3,2,1,0,1,2,2,2,0,1,3,3,0,2,1,3,3,2,0,0,3,0,3,2,1,3,2,0,0,1,2,1,2,0,1,2,2,2,0,1,3,3,0,0,1,1,2,2,2,1,3,0,0,0,1,2,3,4,0,0,2,2,0,2,1,3,0,0,2,1,3,3,0,0,1,2,1,3,0,1,3,0,2,0,1,2,3,0,2,1,3,3,0,0,1,2,3,0,0,1,3,2,0,1,1,3,3,0,0,0,2,2,2,0,1,3,3,0,0,1,2,3,0,0,1,2,1,1,2,1,3,2,2,0,1,3,0,2,0,1,3,3,2,0,1,3,0,3,2,1,3,3,0,2,1,2,2,0,2,0,2,2,2,0,1,2,3,0,0,1,3,2,2,0,0,2,3,0,0,1,3,3,4,2,1,3,2,2,0,1,3,2,2,0,1,2,3,2,0,1,2,3,4,0,1,2,2,2,2,1,3,2,1,1,1,3,3,2,0,0,2,3,0,0,1,3,3,2,0,1,3,2,0,0,1,2,3,4,0,1,3,3,0,0,1,2,2,4,0,0,2,2,0,0,1,2,1,2,0,1,3,0,4,0,1,3,3,0,2,1,3,2,0,0,1,3,2,2,0,1,3,2,1,1,1,3,3,0,0,1,3,2,0,2,1,3,2,2,0,1,2,1,2,0,0,2,2,0,0,1,3,2,2,0,1,3,3,0,2,1,3,3,0,0,1,3,2,0,0,1,3,0,3,0,0,0,3,2,0,1,3,3,0,2,1,3,3,0,0,0,0,2,4,2,1,2,1,4,0,1,3,2,4,0,1,3,2,2,0,1,3,0,4,2,1,3,2,0,0,1,2,2,2,0,0,2,2,2,0,1,3,3,0,0,1,2,1,2,0,1,3,0,4,0,1,3,0,3,0,0,3,3,4,2,1,3,3,4,0,1,3,2,4,0,1,2,3,2,0,1,3,3,0,2,0,0,3,0,0,1,3,0,2,0,1,3,3,0,2,1,2,1,2,0,1,2,2,2,0,0,2,3,0,0,1,1,2,1,1,1,2,2,2,0,0,3,0,3,0,1,3,3,0,0,1,2,1,2,0,1,1,2,1,2,1,3,3,0,0,1,3,3,4,0,1,3,0,0,1,1,3,3,2,0,1,3,0,0,1,1,3,2,2,2,1,3,3,0,0,1,0,3,4,0,0,2,0,0,0,1,2,3,0,1,1,3,3,4,2,1,2,1,2,0,1,3,2,2,0,1,3,3,0,0,1,2,1,3,0,1,2,2,2,0,0,2,2,2,0,1,3,3,0,0,1,3,2,4,2,1,2,2,2,2,1,3,3,0,0,1,3,2,0,0,1,3,2,0,1,1,2,1,2,2,1,3,3,2,0,1,3,3,4,0,0,0,3,0,0,1,2,2,2,0,1,3,3,2,2,1,3,3,0,0,1,3,3,0,0,1,3,2,2,0,1,3,3,4,0,1,0,3,2,2,1,3,2,1,0,0,3,2,4,0,1,3,2,2,0,1,3,0,0,2,0,2,2,0,0,1,3,3,2,0,1,3,3,0,0,1,3,2,0,0,1,0,3,0,0,0,3,3,0,0,1,3,3,2,2,1,3,3,0,0,1,3,2,0,0,1,2,3,2,0,1,3,3,0,2,1,3,0,2,2,1,3,2,0,0,1,3,2,2,2,1,3,3,4,0,0,0,3,2,0,1,3,3,2,0,1,2,3,0,0,1,3,0,1,2,1,0,3,0,0,1,2,3,2,2,1,3,3,0,2,1,0,3,1,2,1,2,3,0,0,1,3,2,0,0,1,3,0,0,0,1,3,2,0,2,1,3,0,2,0,1,2,3,0,0,1,3,3,2,0,0,3,0,1,0,1,3,3,4,2,1,3,2,2,0,1,3,0,3,2,1,2,1,2,0,1,3,3,2,0,1,3,3,0,2,1,2,2,2,0,1,2,2,1,1,1,3,3,0,0,1,3,2,2,0,1,2,1,3,0,1,3,3,0,0,1,3,3,0,0,1,2,1,2,2,1,3,2,2,0,1,3,3,4,0,1,3,0,4,0,1,3,3,2,2,1,3,3,0,0,1,3,3,2,0,1,3,2,2,0,0,2,3,0,0,1,0,2,2,0,1,2,3,0,2,1,2,2,0,2,1,3,2,4,0,1,3,3,2,2,1,3,2,1,1,1,3,3,0,2,1,0,3,1,0,1,3,2,2,0,1,2,1,4,2,1,2,1,2,0,1,2,3,0,0,1,3,2,4,2,1,3,2,2,0,1,2,2,1,1,1,3,2,2,2,0,0,3,0,0,1,3,2,2,0,1,2,1,2,0,1,3,3,0,0,1,3,2,2,0,1,0,3,2,0,1,3,0,3,0,0,1,2,2,0,1,2,1,2,2,0,3,0,0,2,0,1,2,4,0,0,2,3,2,0,1,3,2,4,0,1,3,2,2,0,1,3,0,2,2,1,3,2,2,2,1,3,3,2,0,1,1,2,2,0,1,3,3,0,0,1,3,2,2,2,0,0,3,2,0,1,3,3,0,0,1,3,2,1,0,0,3,3,0,0,1,3,2,1,0,0,3,3,0,0,1,3,3,0,0,1,3,3,0,0,1,3,3,2,0,1,3,3,2,0,1,2,3,0,0,0,3,3,4,0,1,3,2,4,0,1,2,1,2,0,1,3,3,2,0,0,3,2,4,0,0,0,3,0,2,0,2,2,2,0,1,3,3,4,2,1,2,2,2,2,1,1,3,4,2,1,3,0,1,0,1,3,2,2,0,1,3,3,0,0,1,3,2,2,2,1,1,2,4,0,1,3,2,0,1,0,3,0,3,0,1,3,2,1,0,0,3,2,0,1,1,3,2,4,2,1,3,0,1,1,0,3,3,2,0,0,3,3,0,0,0,3,3,4,2,0,2,2,2,2,1,3,3,0,2,0,0,3,0,2,0,2,3,2,0,1,2,1,3,2,1,3,3,2,2,1,3,3,4,2,1,2,1,2,0,1,3,3,0,0,1,1,2,2,0,0,2,3,4,0,0,2,0,3,0,1,3,0]

type Point = (Int, Int)

moveWithJet :: Hashset.HashSet Point -> [Point -> [Point]] -> Point -> [Move] -> [Int]
moveWithJet rocks [] _ _ = []
moveWithJet rocks generators@(g:gs) pos@(x, y) (move:moves)
  | canMove = moveWithGravity rocks generators newPosition moves
  | otherwise = moveWithGravity rocks generators pos moves
  where
    newPosition = nextPosition move pos
    newShape = g newPosition
    canMove = List.all (isValid rocks) newShape

moveWithGravity rocks generators@(g:gs) pos@(x, y) moves
  | canMove = moveWithJet rocks generators newPosition moves
  | otherwise = (newMaxY - oldMaxY) : moveWithJet newRocks gs (2, newMaxY + 4) moves
  where
    newPosition = (x, y - 1)
    newShape = g newPosition
    canMove = List.all (isValid rocks) newShape

    newRocks = addShape rocks g pos

    oldMaxY = higherY rocks
    newMaxY = higherY newRocks

higherY :: Hashset.HashSet (Int, Int) -> Int
higherY = maximum . (++ [0]) . map snd . toList

nextPosition MoveLeft (x, y) = (x - 1, y)
nextPosition MoveRight (x, y) = (x + 1, y)

isValid rocks pos@(x, y) = x >= 0 && x <= 6 && y >= 0 && not (member pos rocks)

addShape set g pos = fromList (g pos) `union` set

draw :: Hashset.HashSet (Int, Int) -> [Int] -> [Char]
draw rocks = concatMap (drawSingle rocks . swap . (`divMod` 7))

drawSingle rocks pos@(x, y)
  | x == 0 = "|" ++ symbol
  | x == 6 = symbol ++ "|\n"
  | otherwise = symbol
  where
    isRock = member pos rocks
    symbol = if isRock then "#" else "."

shape1 (x, y) = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]
shape2 (x, y) = [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 1, y + 2)]
shape3 (x, y) = [(x, y), (x + 1, y), (x + 2, y), (x + 2, y + 1), (x + 2, y + 2)]
shape4 (x, y) = [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)]
shape5 (x, y) = [(x, y), (x, y + 1), (x + 1, y), (x + 1, y + 1)]

data Move
  = MoveLeft
  | MoveRight
  deriving (Show, Eq)

parse :: String -> [Move]
parse [] = []
parse ('<':rs) = MoveLeft : parse rs
parse ('>':rs) = MoveRight : parse rs
parse (_:rs) = parse rs