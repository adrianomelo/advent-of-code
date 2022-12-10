{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let ops = map parse content
    let opsWithCycle = concatMap opToCycles ops

    let cycles = scanl (+) 1 opsWithCycle
    let cyclesWithIndex = zip [1..] cycles

    let signal = filter (\(i,_) -> i `mod` 40 - 20 == 0) cyclesWithIndex
    let signalSum = sum $ map (uncurry (*)) signal

    let drawing = concatMap draw cyclesWithIndex

    -- print signalSum
    putStr drawing

    hClose handle

data Op
    = NoOp
    | Addx Int
    deriving (Show)

opToCycles (Addx n) = [0, n]
opToCycles _ = [0]

parse ('a':'d':'d':'x':' ':number) = Addx (read number)
parse _ = NoOp

draw (index, spritePosition)
    | spriteInPosition && endOfLine = "#\n"
    | not spriteInPosition && endOfLine = ".\n"
    | spriteInPosition = "#"
    | otherwise = "."
    where
        position = index `mod` 40
        diff = abs (spritePosition - position + 1)
        spriteInPosition = diff < 2
        endOfLine = position == 0

