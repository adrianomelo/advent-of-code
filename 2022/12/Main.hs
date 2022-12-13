{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           Control.Monad    (guard)
import qualified Data.Char        as Char
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet     as HashSet
import qualified Data.List        as List
import qualified Data.Map         as Map
import qualified Data.Maybe       as Maybe
import           Debug.Trace      (traceM)
import           System.IO

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let indexed = indexStringList content
    let start@(startPosition, _) = Maybe.fromJust $ List.find isStart indexed
    let end@(endPosition, _) = Maybe.fromJust $ List.find isEnd indexed
    let nodeMap = Map.fromList indexed

    let findPath = aStar                            -- A Star alg. partially applied (without starting point)
                   (neighbors nodeMap)   -- The graph we are searching through, given as a function from vertices to their neighbours.
                   (\_ _ -> 1)                      -- Distance function between neighbouring vertices of the graph. This will never be applied to vertices that are not neighbours, so may be undefined on pairs that are not neighbours in the graph.
                   (distance endPosition)           -- Heuristic distance to the (nearest) goal. This should never overestimate the distance, or else the path found may not be minimal.
                   (== endPosition)                 -- The goal, specified as a boolean predicate on vertices.

    -- part 1
    -- print $ length $ Maybe.fromJust $ findPath startPosition

    -- part 2
    let starts = map fst . filter isA $ indexed
    print $ minimum $ map length $ Maybe.mapMaybe findPath starts

    hClose handle

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neighbors :: Map.Map (Int, Int) Int -> (Int, Int) -> HashSet.HashSet (Int, Int)
neighbors nodeMap pos = HashSet.fromList filtered
    where
        moves = possibleMoves pos
        filtered = filter (isAllowedToMove nodeMap pos) moves

isAllowedToMove nodeMap from to = isNode && isMovePossible
    where
        isNode = Map.member to nodeMap
        isMovePossible = canMove nodeMap from to

canMove map p1 p2 = (a >= 97 && b >= 97 && (b <= a || (a + 1) == b)) || a == 83 || a == 122 && b == 69
    where
        a =  Map.findWithDefault 0 p1 map
        b =  Map.findWithDefault 0 p2 map

possibleMoves (a, b) = [(a, b + 1), (a, b - 1), (a + 1, b), (a - 1, b)]

isStart (_,c) = c == 83 --'S'
isA (_,c) = c == 97 --'a'
isEnd (_,c) = c == 69 -- 'E'

indexStringList :: [[Char]] -> [((Int, Int), Int)]
indexStringList l = [((i, j), Char.ord char) | (j, oneLine) <- zip [0..] l, (i, char) <- zip [0..] oneLine]
