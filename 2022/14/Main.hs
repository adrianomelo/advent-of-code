{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Attoparsec.Text
    ( choice, sepBy, decimal, char, endOfLine, parseOnly, string )
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (fromRight)
import Data.Text (pack)
import Data.List (sort)
import GHC.OldList (elemIndex)
import Data.Maybe (catMaybes)
import Data.HashSet (fromList, HashSet)


main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content

    let rocks = createRockSet input
    print rocks

    hClose handle


createRockSet :: [[Rock]] -> HashSet Rock
createRockSet = fromList . concatMap buildPaths

buildPaths :: [Rock] -> [Rock]
buildPaths [] = []
buildPaths [_] = []
buildPaths (a:b:rest) = buildPath a b ++ buildPaths (b:rest)

buildPath :: Rock -> Rock -> Path
buildPath (ax, ay) (bx, by)
  | ax == bx = [(ax, y) | y <- [startY..endY]]
  | otherwise = [(x, ay) | x <- [startX..endX]]
  where
    startX = min ax bx
    endX = max ax bx
    startY = min ay by
    endY = max ay by

type Rock = (Int, Int)
type Path = [Rock]

parse :: String -> [[Rock]]
parse = fromRight (error "Invalid input format") . parseOnly (arrows `sepBy` endOfLine) . pack
  where
    point = do
      x <- decimal
      _  <- char ','
      y <- decimal
      return (x, y)

    arrows = do
      point `sepBy` string " -> "
