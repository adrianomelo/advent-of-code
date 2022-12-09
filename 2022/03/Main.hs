{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List
import qualified Data.Char as Char

main :: IO ()
main = do
    handle <- openFile "input2.txt" ReadMode
    content <- lines <$> hGetContents handle

    print $ solve content

    hClose handle

solve [] = 0
solve (a:b:c:rest) = item' a b c + solve rest

item' :: [Char] -> [Char] -> [Char] -> Int
item' a b c = sum $ map toNumber $ nub $ intersect a $ intersect b c


--- PART 1 ---

partOne :: IO ()
partOne = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let sacks = map (\x -> splitAt ((length x) `div` 2) x) content

    print $ findItem sacks

    hClose handle

findItem :: [([Char], [Char])] -> Int
findItem [] = 0
findItem ((a, b):rest) = item a b + findItem rest

item :: [Char] -> [Char] -> Int
item a b = sum $ map toNumber $ nub $ intersect a b

toNumber :: Char -> Int
toNumber a
    | Char.isLower a = Char.ord a - Char.ord 'a' + 1
    | otherwise = Char.ord a - Char.ord 'A' + 27