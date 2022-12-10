{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts  #-}

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

    let ops = map parse content
    let opsWithCycle = concatMap mapToInt ops

    let cycles = scanl (+) 1 opsWithCycle
    let cyclesWithIndex = zip [1..] cycles

    let signal = filter (\(i,_) -> i `mod` 40 - 20 == 0) cyclesWithIndex
    let signalSum = sum $ map (uncurry (*)) signal

    let tick = concatMap draw cyclesWithIndex

    -- print ops
    -- print $ take 20 opsWithCycle
    -- print cycles
    -- print signal
    -- print signalSum
    -- print cyclesWithIndex
    putStr tick

    hClose handle

data Op
    = NoOp
    | Addx Int
    deriving (Show)

mapToInt (Addx n) = [0, n]
mapToInt _ = [0]

parse ('a':'d':'d':'x':' ':number) = Addx (read number)
parse _ = NoOp

draw (a, b)
    | spriteInPosition && endOfLine = "#\n"
    | not spriteInPosition && endOfLine = ".\n"
    | spriteInPosition = "#"
    | otherwise = "."
    where
        position = a `mod` 40
        diff = abs (b - position + 1)
        spriteInPosition = diff < 2
        endOfLine = position == 0

