{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let (input', moves') = break null content

    let moves = map getMoves $ drop 1 moves'
    let input = stacks input'

    print input
    print moves

    let finalStacks = foldl makeMove input moves

    print finalStacks

    print $ concatMap (filter Char.isAlpha . head) finalStacks

    hClose handle

makeMove :: [[[Char]]] -> (Int, Int, Int) -> [[[Char]]]
makeMove initial (qnt, fromIndex, toIndex) = final
    where
        (dropped, newFrom) = splitAt qnt $ initial !! (fromIndex - 1)
        to = initial !! (toIndex - 1)
        -- newTo = reverse dropped ++ to
        -- change to part 2
        newTo = dropped ++ to

        replaceFunc = \(x, y) -> if y == fromIndex then newFrom else if y == toIndex then newTo else x
        final = map replaceFunc $ zip initial [1..]

getMoves :: [Char] -> (Int, Int, Int)
getMoves move = (read a, read b, read c)
    where
        [_, a, _, b, _, c] = words move

stacks :: [[Char]] -> [[[Char]]]
stacks s = map (filter (\(y:x:sx) -> x /= ' ')) $ List.transpose $ map stacks' s

stacks' :: [Char] -> [[Char]]
stacks' (a:b:c:d:rest) = [a, b, c] : stacks' rest
stacks' [a, b, c] = [[a, b, c]]