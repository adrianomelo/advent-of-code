{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import qualified Data.Char  as Char
import           Data.List  (zipWith5)
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord   as Ord
import           System.IO


main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let directions = map parse content
    let moves = concatMap directionToMove directions

    -- part 1
    let ropeMovement = scanl moveHead initialRope moves
    let visited = List.nub $ map snd ropeMovement
    let positions = length visited

    -- part 2
    let ropeMovementN = scanl moveRopeN initialRopeN moves
    let visitedN = List.nub $ map last ropeMovementN
    let positionsN = length visitedN

    -- print directions
    -- print moves
    -- print ropeMovement
    -- print visited
    -- print positions  -- 6745

    --print ropeMovementN
    print positionsN

    hClose handle

data Direction
    = U Int
    | D Int
    | L Int
    | R Int
    | NoOp
    deriving (Show)

type Knot = (Int, Int)
type RopeN = [Knot]

initialRopeN :: RopeN
initialRopeN = replicate 10 (0, 0)

moveRopeN :: RopeN -> Move -> RopeN
moveRopeN ((headX, headY):tail) (x, y) = adjustTailN ((headX + x, headY + y):tail)

adjustTailN :: RopeN -> RopeN
adjustTailN = scanl1 adjustTail'

adjustTail' a b = snd $ adjustTail (a, b)

-- part 1

type Rope = ((Int, Int), (Int, Int))
type Move = (Int, Int)

initialRope :: Rope
initialRope = ((0, 0), (0, 0))

moveHead :: Rope -> Move -> Rope
moveHead ((headX, headY), tail) (x, y) = adjustTail ((headX + x, headY + y), tail)

adjustTail :: Rope -> Rope
adjustTail ((headX, headY), (tailX, tailY))
    | diffX == 2 && diffY == 0  = ((headX, headY), (tailX + 1, tailY))
    | diffX == -2 && diffY == 0 = ((headX, headY), (tailX - 1, tailY))
    | diffX == 0 && diffY == 2  = ((headX, headY), (tailX, tailY + 1))
    | diffX == 0 && diffY == -2 = ((headX, headY), (tailX, tailY - 1))

    | diffX == 2  && diffY == 1  = ((headX, headY), (tailX + 1, tailY + 1))
    | diffX == 2  && diffY == -1 = ((headX, headY), (tailX + 1, tailY - 1))
    | diffX == -2 && diffY == 1  = ((headX, headY), (tailX - 1, tailY + 1))
    | diffX == -2 && diffY == -1 = ((headX, headY), (tailX - 1, tailY - 1))
    | diffX == 1  && diffY == 2  = ((headX, headY), (tailX + 1, tailY + 1))
    | diffX == -1 && diffY == 2  = ((headX, headY), (tailX - 1, tailY + 1))
    | diffX == 1  && diffY == -2 = ((headX, headY), (tailX + 1, tailY - 1))
    | diffX == -1 && diffY == -2 = ((headX, headY), (tailX - 1, tailY - 1))

    -- new moves, part 2
    | diffX == 2 && diffY == 2   = ((headX, headY), (tailX + 1, tailY + 1))
    | diffX == 2 && diffY == -2  = ((headX, headY), (tailX + 1, tailY - 1))
    | diffX == -2 && diffY == 2  = ((headX, headY), (tailX - 1, tailY + 1))
    | diffX == -2 && diffY == -2 = ((headX, headY), (tailX - 1, tailY - 1))

    | otherwise = ((headX, headY), (tailX, tailY))
    where
        diffX = headX - tailX
        diffY = headY - tailY

directionToMove :: Direction -> [Move]
directionToMove (R n) = replicate n (1, 0)
directionToMove (L n) = replicate n (-1, 0)
directionToMove (U n) = replicate n (0, 1)
directionToMove (D n) = replicate n (0, -1)
directionToMove NoOp  = []

parse :: String -> Direction
parse ('R':' ':steps) = R (read steps)
parse ('L':' ':steps) = L (read steps)
parse ('U':' ':steps) = U (read steps)
parse ('D':' ':steps) = D (read steps)
parse _               = NoOp
