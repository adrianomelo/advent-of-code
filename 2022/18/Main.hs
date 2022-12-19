{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import Data.List ( sort, group, nub )
import Data.HashSet (fromList, empty, member, intersection, union, toList, HashSet, size)
import Data.Tuple (swap)
import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string, signed)
import Data.Either (fromRight)
import Data.Text (pack)
import Data.Ix (inRange)
import GHC.Ix (range)
import Control.Monad (guard)
import Data.HashSet (insert)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content
    let droplet = fromList input

    let faces = part1 droplet input
    
    print $ "faces: " ++ show faces

    let range'@((minX, maxX), (minY, maxY), (minZ, maxZ)) = boundingBox input
    print $ "range: " ++ show range'

    -- let volume = (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)
    -- print $ "volume: " ++ show volume

    let outside = countOutside range' droplet empty [(minX, minY, minZ)]
    print $ "outside: " ++ show outside

    -- print $ volume - outside

    hClose handle

-- airTrapped cubes = map head . filter (\x -> 6 == length x) . nub . group . filter (not . isCube cubes) . sort . concatMap neighbours

type Rng = ((Int, Int), (Int, Int), (Int, Int))

countOutside :: Rng -> HashSet Point -> HashSet Point -> [Point] -> Int
countOutside _ _ _ [] = 0
countOutside range' droplet visited (point:points) = dropletFaceCount + countOutside range' droplet visited' (air ++ points)
  where
    wasVisited = not $ isNew visited point
    n = filter (isNew visited) . neighbours $ point
    air = filter (isAir droplet range') n
    dropletFaceCount = if wasVisited then 0 else length . filter (isDroplet droplet) $ n
    visited' = insert point visited

isAir :: HashSet Point -> Rng -> Point -> Bool
isAir droplet range' point = range' `inRange'` point && not (member point droplet)

isNew :: HashSet Point -> Point -> Bool
isNew visited point = not $ member point visited

isDroplet :: HashSet Point -> Point -> Bool
isDroplet droplet point = member point droplet

inRange' :: Rng -> Point -> Bool
inRange' (rangeX, rangeY, rangeZ) (x, y, z) = rangeX `inRange` x && rangeY `inRange` y && rangeZ `inRange` z

boundingBox input = ((minX, maxX), (minY, maxY), (minZ, maxZ))
  where
    (xs, ys, zs) = unzip3 input
    minX = minimum xs - 1
    maxX = maximum xs + 1
    minY = minimum ys - 1
    maxY = maximum ys + 1
    minZ = minimum zs - 1
    maxZ = maximum zs + 1

part1 :: HashSet Point -> [Point] -> Int
part1 cubes = foldl (\x cube -> x + countFacesSingle cubes cube) 0

countFacesSingle :: HashSet Point -> Point -> Int
countFacesSingle cubes cube = 6 - length (filter (isCube cubes) $ neighbours cube)

isCube set position = member position set

neighbours (x, y, z) = [ (x + 1, y, z)
                       , (x - 1, y, z)
                       , (x, y + 1, z)
                       , (x, y - 1, z)
                       , (x, y, z + 1)
                       , (x, y, z - 1)]

type Point = (Int, Int, Int)

parse :: String -> [Point]
parse = fromRight (error "Invalid input format") . parseOnly readings . pack
  where
    point = do
      x <- decimal
      _  <- char ','
      y <- decimal
      _ <- char ','
      z <- decimal
      return (x, y, z)

    readings = do
      point `sepBy` endOfLine