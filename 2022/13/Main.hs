{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Attoparsec.Text
    ( choice, sepBy, decimal, char, endOfLine, parseOnly )
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (fromRight)
import Data.Text (pack)
import Data.List (sort)
import GHC.OldList (elemIndex)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents handle

    let list = parse content
    -- part 1
    print $ part1 list

    print $ part2 ((divider1, divider2) : list)

    hClose handle

part2 ::  [Packet] -> Int
part2 list = product $ map (+ 1) $ catMaybes [first, second]
    where
        sorted = sort $ concatMap (\(a,b) -> [a,b]) list
        first = elemIndex divider1 sorted
        second = elemIndex divider2 sorted

divider1 = PList [PList [Element 2]]
divider2 = PList [PList [Element 6]]

part1 list = sum $ map fst $ filter snd $ zip [1..] $ map (uncurry (<=)) list

data PList = PList [PList] | Element Int deriving (Eq)
type Packet = (PList, PList)

instance Ord PList where
  compare :: PList -> PList -> Ordering
  compare (PList []) (PList []) = EQ
  compare (PList []) (PList  _) = LT
  compare (PList  _) (PList []) = GT
  compare (PList (l:ls)) (PList (r:rs)) = case compare l r of
    EQ  -> compare ls rs
    out -> out
  compare (Element l) (Element r) = compare l r
  compare (Element l) (PList rs) = compare (PList [Element l]) (PList rs)
  compare (PList ls) (Element r) = compare (PList ls) (PList [Element r])

instance Show PList where
  show (Element e) = show e
  show (PList xs) = show xs

parse :: String -> [Packet]
parse = fromRight (error "Invalid input format") . parseOnly (packet `sepBy` emptyLine) . pack
  where
    packet = do
      left <- list
      _  <- endOfLine
      right <- list
      return (left, right)

    list = do
      _   <- char '['
      elementOrList <- choice [list, element] `sepBy` char ','
      _   <- char ']'
      return $ PList elementOrList

    element = Element <$> decimal

    emptyLine = do
      _ <- endOfLine
      endOfLine