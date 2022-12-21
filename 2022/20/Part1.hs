{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string, signed)
import Data.Either (fromRight)
import Data.Text (pack)
import Data.List (sortOn, sort, elemIndex)
import Data.Maybe (fromJust)
import Data.Sequence (elemIndexL, deleteAt, insertAt, fromList)
import Data.Foldable (toList)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content
    let size = length input
    let list = [0..(size-1)]
    let seq = fromList list

    let newOrder = reorder input seq list
    -- let newList = map (map (input !!) . toList . snd) newOrde
    let newList = map (input !!) $ toList newOrder

    let zeroIndex = fromJust $ 0 `elemIndex` newList
    let part1 = sum $ map ((newList !!) . (`mod` size) . (+ zeroIndex)) [1000, 2000, 3000]

    print part1

    hClose handle


reorder original seq [] = seq
reorder original seq (i:is) = reorder original seq'' is
  where
    value = original !! i
    index = fromJust $ elemIndexL i seq
    seq' = deleteAt index seq
    inc = (index + value) `mod` length seq'

    nexIndex = if value < 0 && inc <= 0 then length seq' - inc else inc
    seq'' = insertAt nexIndex i seq'


parse :: String -> [Int]
parse = fromRight (error "Invalid input format") . parseOnly readings . pack
  where
    point = do
      signed decimal

    readings = do
      point `sepBy` endOfLine