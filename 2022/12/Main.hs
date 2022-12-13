{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

-- import qualified Data.Char  as Char
-- import           Data.List  (zipWith5)
-- import qualified Data.List  as List
-- import qualified Data.Map   as Map
-- import qualified Data.Maybe as Maybe
import qualified Data.Ord   as Ord
import           System.IO
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.HashPSQ as Queue
-- import qualified Data.PSQueue as PSQ
import Control.Monad (guard)
import Debug.Trace (traceM)

-- type Item = (Int, Int, Char)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    -- let ln = zip [0..] content
    -- let indexed = [(i, j, char) | (i, oneLine) <- zip [0..] content, (j, char) <- zip [0..] oneLine]

    let indexed = indexStringList content
    let start@(startPosition, _) = Maybe.fromJust $ List.find isStart indexed
    let end@(endPosition, _) = Maybe.fromJust $ List.find isEnd indexed

    let nodeMap = Map.fromList indexed
    let visitedSet = Set.singleton (0, 0)

    print nodeMap
    let queue = Queue.singleton startPosition 0 0
    -- let path = walkNodes nodeMap visitedSet endPosition queue
    let path = walkNodes nodeMap visitedSet endPosition [(0, startPosition)]

    -- print $ 1000 - path
    print path

    hClose handle

walkNodes :: Map.Map (Int, Int) Int -> Set.Set (Int, Int) -> (Int, Int) -> [(Int, (Int, Int))] -> Int
walkNodes nodeMap visitedSet end (n@(dist, nPos):ns)
    | end == nPos = dist
    | otherwise = walkNodes nodeMap newVisitedSet end ns'
    where
        -- (nextKey, nextPriority, nextValue) = Maybe.fromJust $ Queue.findMin queue
        nextMoves = possibleMoves nPos
        allowedMoves = filter (isAllowedToMove nodeMap visitedSet nPos) nextMoves
        allowedMovesWithDist = map (\x -> (dist + 1, x)) allowedMoves
        newVisitedSet = Set.insert nPos visitedSet

        ns' = ns ++ allowedMovesWithDist

        -- newQueue' = Queue.deleteMin queue
        -- newQueue = List.foldl (\q k -> Queue.insert k (nextPriority+1) (nextPriority+1) q) queue allowedMoves

        -- newNs = ns ++ allowedMovesWithDist
        -- newNs = List.sortOn snd newNs'

-- walkNodes :: Map.Map (Int, Int) Int -> Set.Set (Int, Int) -> (Int, Int) -> Queue.HashPSQ (Int, Int) Int Int -> Int
-- walkNodes nodeMap visitedSet end queue
--     | end == nextKey = nextPriority
--     | otherwise = walkNodes nodeMap newVisitedSet end newQueue
--     where
--         (nextKey, nextPriority, nextValue) = Maybe.fromJust $ Queue.findMin queue
--         nextMoves = possibleMoves nextKey
--         allowedMoves = filter (isAllowedToMove nodeMap visitedSet nextKey) nextMoves
--         -- allowedMovesWithDist = map (\x -> (x, dist + 1)) allowedMoves
--         newVisitedSet = Set.insert nextKey visitedSet

--         newQueue' = Queue.deleteMin queue
--         newQueue = List.foldl (\q k -> Queue.insert k (nextPriority+1) (nextPriority+1) q) queue allowedMoves

--         -- newNs = ns ++ allowedMovesWithDist
--         -- newNs = List.sortOn snd newNs'


isAllowedToMove nodeMap visitedSet from to = notVisited && isNode && isMovePossible
    where
        notVisited = Set.notMember to visitedSet
        isNode = Map.member to nodeMap
        isMovePossible = canMove nodeMap from to

-- walkNodes nodes visited start end = do
--     nextNode <- possibleMoves start

--     guard $ Set.notMember nextNode visited
--     guard $ Map.member nextNode nodes
--     guard $ canMove nodes start nextNode

--     if nextNode == end
--         then return [nextNode]
--         else do
--             let nextVisited = Set.insert nextNode visited
--             next <- walkNodes nodes nextVisited nextNode end
--             return $ nextNode : next

-- canMove :: p -> a -> a -> Bool
canMove map p1 p2 = (a >= 97 && b >= 97 && (b <= a || (a + 1) == b)) || a == 83 || a == 122 && b == 69
    where
        a =  Map.findWithDefault 0 p1 map
        b =  Map.findWithDefault 0 p2 map

possibleMoves (a, b) = [(a, b - 1), (a, b + 1), (a - 1, b), (a + 1, b)]

isStart (_,c) = c == 83 --'S'
isEnd (_,c) = c == 69 -- 'E'

indexStringList :: [[Char]] -> [((Int, Int), Int)]
indexStringList l = [((i, j), Char.ord char) | (j, oneLine) <- zip [0..] l, (i, char) <- zip [0..] oneLine]
