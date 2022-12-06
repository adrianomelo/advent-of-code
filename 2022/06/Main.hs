{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let starts = map (signal 4) content
    let starts' = map (signal' 0 14) content

    print starts'

    hClose handle

signal :: Int -> [Char] -> Int
signal index (a:b:c:d:rest)
    | countDifferent == 4 = index
    | otherwise = signal (index + 1) (b:c:d:rest)
    where
        countDifferent = length $ List.nub [a, b, c, d]


signal' :: Int -> Int -> [Char] -> Int
signal' start size list
    | countDifferent == size = start + size
    | otherwise = signal' (start + 1) size list
    where
        countDifferent = length $ List.nub $ take size $ drop start list 
