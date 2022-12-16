{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string, signed)
import      qualified     Data.HashSet         as Hashset
import qualified Data.HashMap.Lazy as HashMap
import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import Data.List (nub, find)
import qualified Data.Range as Range
import Data.Either (fromRight)
import Data.Text (pack)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content


    let maxRange = [0 Range.+=+ maxY]
    -- let a = findPoint input maxRange [0..maxY]
    -- print a

    print $ rangesGivenY 3363767 input -- [3157536 +=+ 4000000,0 +=+ 3157534]

    hClose handle


maxY = 4000000

findPoint :: [Pair] -> [Range.Range Int] ->  [Int] -> Maybe Int
findPoint input maxRange = find (\y -> rangesGivenY y input /= maxRange)

rangesGivenY :: Int -> [Pair] -> [Range.Range Int]
rangesGivenY y = Range.joinRanges . Range.mergeRanges . map (rangeGivenY y)

rangeGivenY :: Int -> Pair -> Range.Range Int
rangeGivenY y (sensor@(sx, sy), beacon@(bx, by))
  | distanceY > d = 0 Range.+=+ 0
  | otherwise = startX Range.+=+ endX
  where
    distanceY = abs (y - sy)
    d = distance sensor beacon
    startX = max (sx - d + distanceY) 0
    endX = min (sx + d - distanceY) maxY

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

type Point = (Int, Int)
type Pair = (Point, Point)

parse :: String -> [Pair]
parse = fromRight (error "Invalid input format") . parseOnly readings . pack
  where
    point = do
      _ <- string "Sensor at x="
      sx <- signed decimal
      _  <- string ", y="
      sy <- signed decimal
      _ <- string ": closest beacon is at x="
      bx <- signed decimal
      _  <- string ", y="
      by <- signed decimal
      return ((sx, sy), (bx, by))

    readings = do
      point `sepBy` endOfLine
