{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string, signed)
import           Data.Either          (fromRight)
import           Data.HashSet         (HashSet, empty, fromList, insert, member,
                                       size, toList, difference)
import qualified Data.HashMap.Lazy as HashMap
import           Data.List            (sort, find)
import           Data.Maybe           (catMaybes)
import           Data.Text            (pack)
import           GHC.OldList          (elemIndex)
import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import Data.List (nub)
import qualified Data.Range as Range

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content

    let targetY = 2000000
    let minX = 0
    let maxX = 4000000

    -- let ranges = map (rowRange input) [0..maxX]

    let beacons = map snd input
    let sensors = map fst input
    let hardwareXs = fromList $ map fst $ filter (\(x, y) -> y == targetY) $ beacons ++ sensors

    let signalX = fromList $ concatMap (toRange targetY) input

    -- let hardwareXs = HashMap.fromList input

    -- let invertedIndexes = map (`divMod` maxX) [0..(maxX * maxX)]

    let computedRange = map (\(s, b) -> (s, distance s b)) input
    -- let spot = find (\(y, x) -> outOfRange (x, y) computedRange) invertedIndexes
    -- print spot

    print $ size $ difference signalX hardwareXs

    hClose handle




toRotPoint (x, y) = (x + y, y - x)

toOriginalPoint (x, y) = ((x + y) / 2, (y - x) / 2)


-- outOfRange :: (Int, Int) -> [((Int, Int), Int)] -> Bool
-- outOfRange point = all (\(sensor, range) -> distance sensor point > range)

part1 = length . filter ((== 2000000) . snd) . toList

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

toRange targetY (sensor@(sx, sy), beacon) = [x | x <- [(sx - rangeX) .. (sx + rangeX)]
                                            , distance sensor (x, targetY) <= d]
  where
    d = distance sensor beacon
    diff = d - (abs targetY - sy)
    rangeX = min d diff

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
