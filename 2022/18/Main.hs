{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import Data.List ( sort, group, nub )
import Data.HashSet (fromList, empty, member, intersection, union, toList, HashSet)
import Data.Tuple (swap)
import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string, signed)
import Data.Either (fromRight)
import Data.Text (pack)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content
    let cubes = fromList input

    let faces = countFaces cubes input
    print faces

    let trapped = airTrapped cubes input
    print trapped
    -- print $ nub . group . filter (not . isCube cubes) . sort . concatMap neighbours $ input

    -- print $ faces - allNeighbours -- not 4434

    hClose handle

airTrapped cubes = map head . filter (\x -> 6 == length x) . nub . group . filter (not . isCube cubes) . sort . concatMap neighbours

countFaces :: HashSet Point -> [Point] -> Int
countFaces cubes = foldl (\x cube -> x + countFacesSingle cubes cube) 0

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