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

    let part1Gen = take 2022 $ cycle [shape1, shape2, shape3, shape4, shape5]
    let part1Rocks = moveWithJet empty part1Gen (2, 3) input
    let part1 = higherY part1Rocks
    -- putStrLn $ draw part1Rocks [0..(30 * 7 - 1)]
    print (part1 + 1)

    hClose handle

type Point = (Int, Int)

moveWithJet :: Hashset.HashSet Point -> [Point -> [Point]] -> Point -> [Move] -> Hashset.HashSet Point
moveWithJet rocks [] _ _ = rocks
moveWithJet rocks generators@(g:gs) pos@(x, y) (move:moves)
  | canMove = moveWithGravity rocks generators newPosition moves
  | otherwise = moveWithGravity rocks generators pos moves
  where
    newPosition = nextPosition move pos
    newShape = g newPosition
    canMove = List.all (isValid rocks) newShape

moveWithGravity rocks generators@(g:gs) pos@(x, y) moves
  | canMove = moveWithJet rocks generators newPosition moves
  | otherwise = moveWithJet newRocks gs (2, newMaxY + 4) moves
  where
    newPosition = (x, y - 1)
    newShape = g newPosition
    canMove = List.all (isValid rocks) newShape

    newRocks = addShape rocks g pos
    newMaxY = higherY newRocks

higherY :: Hashset.HashSet (Int, Int) -> Int
higherY = maximum . map snd . toList

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