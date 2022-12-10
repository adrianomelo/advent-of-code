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

    -- let mapped = visibleTrees content
    -- let number = length $ filter not (concat mapped)

    let leftToRight = map find content
    let rightToLeft = map (reverse . find . reverse) content

    let transposed = List.transpose content

    let topToBottom = List.transpose $ map find transposed
    let bottomToTop = List.transpose $ map (reverse . find . reverse) transposed

    let scenic = List.zipWith4 (List.zipWith4 scenicFactor) leftToRight rightToLeft topToBottom bottomToTop

    let best = List.maximum $ List.concat scenic

    -- print number

    -- print content
    -- print leftToRight
    -- print rightToLeft
    -- print transposed
    -- print topToBottom
    -- print bottomToTop
    -- print scenic
    print best

    hClose handle

-- scenicFactor :: Char -> Char -> Char -> Char  -> Int
scenicFactor lr rl tb bt = List.product [lr, rl, tb, bt]

find :: String -> [Int]
find [] = []
find (a:as) = distance : find as
    where
        nextBig = (+ 1) $ length $ takeWhile (< a) as
        distance = min nextBig (length as)

visibleTrees :: [[Char]] -> [[Bool]]
visibleTrees content' = mapped
    where
        content = map (map (\x ->Char.intToDigit $ 1 + Char.digitToInt x)) content'

        transposed = List.transpose content

        leftToRight = map (scanl max '0') content
        rightToLeft = map (scanr max '0' . drop 1) content

        topToBottom = List.transpose $ map (scanl max '0') transposed
        bottomToTop = List.transpose $ map (scanr max '0' . drop 1) transposed

        mapped = zipWith5 check content leftToRight rightToLeft topToBottom bottomToTop

check :: [Char] -> [Char] -> [Char] -> [Char] -> [Char] -> [Bool]
check = zipWith5 isHiddenTree

isHiddenTree :: Char -> Char -> Char -> Char -> Char -> Bool
isHiddenTree item lr rl tb bt = item <= elementToCompare
    where
        elementToCompare = List.minimum [lr, rl, tb, bt]


