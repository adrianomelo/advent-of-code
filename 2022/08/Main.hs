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
    content' <- lines <$> hGetContents handle

    let content = map (map (\x ->Char.intToDigit $ 1 + Char.digitToInt x)) content'

    -- let head' = map head content
    let transposed = List.transpose content

    let leftToRight = map (scanl max '0') content
    let rightToLeft = map (scanr max '0' . drop 1) content

    let topToBottom = List.transpose $ map (scanl max '0') transposed
    let bottomToTop = List.transpose $ map (scanr max '0' . drop 1) transposed


    let mapped = zipWith5 check content leftToRight rightToLeft topToBottom bottomToTop

    let number = length $ filter not (concat mapped)


    -- print content
    -- print leftToRight
    -- print rightToLeft
    -- print topToBottom
    -- print bottomToTop
    -- print mapped
    print number

    hClose handle


check :: [Char] -> [Char] -> [Char] -> [Char] -> [Char] -> [Bool]
check = zipWith5 isHiddenTree

isHiddenTree :: Char -> Char -> Char -> Char -> Char -> Bool
isHiddenTree item lr rl tb bt = item <= elementToCompare
    where
        elementToCompare = List.minimum [lr, rl, tb, bt]


