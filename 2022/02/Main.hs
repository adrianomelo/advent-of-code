{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    -- print content
    print $ score content
    print $ score' content

    --print $ maximum output

    hClose handle

score [] = 0
score ("A X":rest) = 1 + 3 + score rest -- rock rock
score ("B X":rest) = 1 + 0 + score rest -- paper rock
score ("C X":rest) = 1 + 6 + score rest -- scissors rock
score ("A Y":rest) = 2 + 6 + score rest -- rock paper
score ("B Y":rest) = 2 + 3 + score rest -- paper paper
score ("C Y":rest) = 2 + 0 + score rest -- scissors paper
score ("A Z":rest) = 3 + 0 + score rest -- rock scissors
score ("B Z":rest) = 3 + 6 + score rest -- paper scissors
score ("C Z":rest) = 3 + 3 + score rest -- sicssors scissors
score _ = 0 -- ignore


score' [] = 0
score' ("A X":rest) = 3 + 0 + score' rest -- rock scissors
score' ("B X":rest) = 1 + 0 + score' rest -- paper rock
score' ("C X":rest) = 2 + 0 + score' rest -- scissors papers
score' ("A Y":rest) = 1 + 3 + score' rest -- rock rock
score' ("B Y":rest) = 2 + 3 + score' rest -- paper paper
score' ("C Y":rest) = 3 + 3 + score' rest -- scissors scissors
score' ("A Z":rest) = 2 + 6 + score' rest -- rock paper
score' ("B Z":rest) = 3 + 6 + score' rest -- paper scissors
score' ("C Z":rest) = 1 + 6 + score' rest -- sicssors rock
score' _ = 0 -- ignore