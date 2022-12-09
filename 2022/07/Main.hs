{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import qualified Data.Char  as Char
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord   as Ord
import           System.IO

type Size = Int
type DirectoryName = String
type FileName = String
type Path = [DirectoryName]
type Node = (Path, FileName, Size)

data Command
    = Cd DirectoryName
    | Ls
    | Dir DirectoryName
    | File FileName Size
    deriving (Show)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let commands = map parseCommand content
    let (_, tree) = foldl readCommand ([], []) commands
    let directoriesMap = foldl du Map.empty tree

    let directories = Map.keys directoriesMap
    let dirAcc = map (pathSum directoriesMap) directories

    let lessThan = filter (<= 100000) dirAcc

    let dirs = map (\z -> (z, pathSum directoriesMap z)) directories

    let dirsDown = List.sortOn (Ord.Down . biggerFirst) dirs
    let dirsUp = List.reverse dirsDown

    let (_, rootSize) = head dirsDown
    print rootSize

    let diskLeft = 70000000 - rootSize

    print $ List.find (\(_, y) -> 30000000 < diskLeft + y) dirsUp

    -- print commands
    -- print directories
    -- print tree
    -- print directoriesMap
    -- print directories
    -- print dirAcc
    -- print lessThan

    -- part 1
    print $ sum lessThan

    hClose handle

biggerFirst (k, v) = v

pathSum :: Map.Map Path Int -> Path -> Int
pathSum map path = sum values
    where
        isChild k v = path `List.isPrefixOf` k
        children = Map.filterWithKey isChild map
        values = Map.elems children

du :: Map.Map Path Int -> Node -> Map.Map Path Int
du map (path', _, size) = updateMap map path' size
    where
        u v = Maybe.Just $ Maybe.maybe size (+ size) v
        updateMap map path size = Map.alter u path map

readCommand :: (Path, [Node]) -> Command -> (Path, [Node])
readCommand (path, nodes) (Cd "..") = (dropLast path, nodes)
readCommand (path, nodes) (Cd "/") = (["/"], nodes)
readCommand (path, nodes) (Cd directoryName) = (path ++ [directoryName], nodes ++ [(path ++ [directoryName], ".", 0)])
readCommand (path, nodes) (File name size) = (path, nodes ++ [(path, name, size)])
readCommand current _ = current

parseCommand :: String -> Command
parseCommand "$ ls"                     = Ls
parseCommand ('$':' ':'c':'d':' ':name) = Cd name
parseCommand ('d':'i':'r':' ':name)     = Dir name
parseCommand fileLine                   = File name size
    where
        (size', name) = break (== ' ') fileLine
        size = read size' :: Int

dropLast :: [a] -> [a]
dropLast path = take (length path - 1) path
