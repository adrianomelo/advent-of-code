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

    let lists = map toSeq content

    print lists
    let part1 = length $ filter id $ map isSubList lists

    print part1
    
    let part2 = length $ filter overlap lists
    print part2

    hClose handle

overlap (a, b) = [] /= List.intersect a b

toSeq :: [Char] -> ([Int], [Int])
toSeq line = (l1, l2)
    where
        (a, a') = span Char.isDigit line
        (b, b') = span Char.isDigit $ drop 1 a'
        (c, c') = span Char.isDigit $ drop 1 b'
        (d, d') = span Char.isDigit $ drop 1 c'

        l1 = [read a..read b]
        l2 = [read c..read d]


toSeq' :: [Int] -> ([Int], [Int])
toSeq' (a:b:c:d:_) = ([a..b], [c..d])

isSubList (a, b) = List.isSubsequenceOf a b || List.isSubsequenceOf b a