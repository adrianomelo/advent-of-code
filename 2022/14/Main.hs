{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string)
import           Data.Either          (fromRight)
import           Data.HashSet         (HashSet, empty, fromList, insert, member,
                                       size)
import           Data.List            (sort)
import           Data.Maybe           (catMaybes)
import           Data.Text            (pack)
import           GHC.OldList          (elemIndex)
import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)


main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = concatMap buildPaths $ parse content

    let rockSet = fromList input
    let bb@(_, (_, maxY)) = boundingBox input

    -- print maxY
    let initialSandSet = empty --fromList [(500,8), (499,8), (501, 8),(500,7)]

    let part1 = fall rockSet initialSandSet maxY (500, 0)
    -- putStrLn $ draw bb rockSet part1
    -- print $ size part1

    let part2 = fall' rockSet initialSandSet maxY (500, 0)
    -- putStrLn $ draw bb rockSet part2
    print $ size part2

    hClose handle


fall' :: HashSet Point -> HashSet Point -> Int -> Rock -> HashSet Point
fall' rockSet sandSet maxY item@(x, y)
  | containsFirst = sandSet
  | canMoveDown = fall' rockSet sandSet maxY down
  | canMoveDownLeft = fall' rockSet sandSet maxY downLeft
  | canMoveDownRight = fall' rockSet sandSet maxY downRight
  -- | otherwise = insert item sandSet
  | otherwise = fall' rockSet (insert item sandSet) maxY (500, 0)
  where
    canMove toPos = not (member toPos rockSet || member toPos sandSet || snd toPos == maxY + 2)

    down = (x, y + 1)
    downLeft = (x - 1, y + 1)
    downRight = (x + 1, y + 1)

    canMoveDown = canMove down
    canMoveDownLeft = canMove downLeft
    canMoveDownRight = canMove downRight

    containsFirst = member (500, 0) sandSet

fall :: HashSet Point -> HashSet Point -> Int -> Rock -> HashSet Point
fall rockSet sandSet maxY item@(x, y)
  | hasFallenIntoAbysm = sandSet
  | canMoveDown = fall rockSet sandSet maxY down
  | canMoveDownLeft = fall rockSet sandSet maxY downLeft
  | canMoveDownRight = fall rockSet sandSet maxY downRight
  -- | otherwise = insert item sandSet
  | otherwise = fall rockSet (insert item sandSet) maxY (500, 0)
  where
    canMove toPos = not (member toPos rockSet || member toPos sandSet)

    down = (x, y + 1)
    downLeft = (x - 1, y + 1)
    downRight = (x + 1, y + 1)

    canMoveDown = canMove down
    canMoveDownLeft = canMove downLeft
    canMoveDownRight = canMove downRight
    hasFallenIntoAbysm = y > maxY

draw :: (Point, Point) -> HashSet Rock -> HashSet Rock -> String
draw ((minX, minY), (maxX, maxY)) rockSet sandSet = unlines output
  where
    rows = [minY..(maxY + 2)]
    cells = [minX..maxX]
    output = map (\y -> map (\x -> drawSingle rockSet sandSet (x,y)) cells) rows

drawSingle :: HashSet Rock -> HashSet Rock -> Point -> Char
drawSingle rockSet sandSet position
  | member position rockSet = '#'
  | member position sandSet = 'o'
  | otherwise = '.'

boundingBox :: [Rock] -> (Rock, Rock)
boundingBox input = ((minX, 0), (maxX, maxY))
  where
    xs = map fst input
    ys = map snd input
    minX = minimum xs
    maxX = maximum xs
    maxY = maximum ys

buildPaths :: [Rock] -> [Rock]
buildPaths []         = []
buildPaths [_]        = []
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
type Point = Rock
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
