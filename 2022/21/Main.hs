{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           System.IO            (IOMode (ReadMode),
                                       NewlineMode (outputNL), hClose,
                                       hGetContents, openFile)
import           Data.Attoparsec.Text (char, choice, decimal, endOfLine,
                                       parseOnly, sepBy, string, signed, number, takeWhile, takeText, space, anyChar)
import Data.Either (fromRight)
import Data.Text (pack, Text)
import Data.Maybe (fromJust)
import Data.HashMap.Strict (fromList, (!), HashMap)
import Data.Foldable (toList)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let input = parse content
    let monkeyMap = fromList input

    let root = monkeyMap ! "root"
    let part1 = eval monkeyMap root

    print part1

    hClose handle

eval :: HashMap Text NodeValue -> NodeValue -> Int
eval monkeyMap (Number a) = a
eval monkeyMap (Op left operation right) = op operation (eval monkeyMap $ monkeyMap ! left) (eval monkeyMap $ monkeyMap ! right)

op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = div

type Monkey = Text
type Operation = Char

data NodeValue
  = Number Int
  | Op Monkey Operation Monkey
  deriving (Show)

type Node = (Monkey, NodeValue)

parse :: String -> [Node]
parse = fromRight (error "Invalid input format") . parseOnly readings . pack
  where
    op = do
      left <- Data.Attoparsec.Text.takeWhile (/= ' ')
      _ <- char ' '
      operation <- anyChar
      _ <- char ' '
      right <- Data.Attoparsec.Text.takeWhile (/= '\n')
      return (Op left operation right)

    value = do
      n <- signed decimal
      return (Number n)

    single = do
      name <- Data.Attoparsec.Text.takeWhile (/= ':')
      _ <- string ": "
      value <- choice [value, op]
      return (name, value)

    readings = do
      single `sepBy` endOfLine