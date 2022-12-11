{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           System.IO
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Tuple (swap)
import Data.Tuple.Extra (fst3)
import Data.Int (Int64)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let l = length operations
    let rounds = take (10000 * l) $ cycle [0..(l-1)]
    let allThrows = initialState ++ throws rounds initialState

    let finalPosition = Map.fromList $ map (\(m,_,id) -> (id, m)) allThrows

    print $ countValues $ map fst3 allThrows
    print $ countValues $ map snd $ Map.toList finalPosition

    hClose handle

data Monkey = Monkey
    { startingItems :: [Int64]
    , operation :: Int64 -> Int64
    , test :: Int64 -> Bool
    , testNumber :: Int64
    , outputTrue :: Int
    , outputFalse :: Int
    }

-- countValues :: (Ord a, Eq a) => [a] -> [(a, Int)]
countValues xs = map (\x -> (head x, length x)) (List.group (List.sort xs))

throws [] state = []
throws (n:ns) state = new ++ throws ns (other ++ new)
    where
        (other, new) = computeState n state

computeState n state = (withOtherMonkeys, newItems)
    where
        (withMonkey, withOtherMonkeys) = List.partition ((== n) . fst3) state
        newItems = map computeNextMove withMonkey

computeNextMove (n, item, id) = (newMonkey, worryLevel, id)
    where
        monkey = operations !! n
        newNumber = operation monkey item
        (_,worryLevel) = fromIntegral newNumber `divMod` divisor
        -- (_,worryLevel) = fromIntegral newNumber `divMod` testNumber monkey
        -- worryLevel = newNumber
        output = test monkey worryLevel
        newMonkey = if output then outputTrue monkey else outputFalse monkey

initialState = [(monkey, item, "id-" ++ show monkey ++ "-" ++ show item) | (monkey, items) <- mapped, item <- items]
    where
        mapped = zipWith (\x op -> (x, startingItems op)) [0..] operations


divisor = List.product $ map testNumber operations 

operations = operations2

operations2 =
    [ Monkey{ startingItems = [89, 74]
            , operation = (* 5)
            , test = \x -> mod x 17 == 0
            , testNumber = 17
            , outputTrue = 4
            , outputFalse = 7
            }
    , Monkey{ startingItems = [75, 69, 87, 57, 84, 90, 66, 50]
            , operation = (+ 3)
            , test = \x -> mod x 7 == 0
            , testNumber = 7
            , outputTrue = 3
            , outputFalse = 2
            }
    , Monkey{ startingItems = [55]
            , operation = (+ 7)
            , test = \x -> mod x 13 == 0
            , testNumber = 13
            , outputTrue = 0
            , outputFalse = 7
            }
    , Monkey{ startingItems = [69, 82, 69, 56, 68]
            , operation = (+ 5)
            , test = \x -> mod x 2 == 0
            , testNumber = 2
            , outputTrue = 0
            , outputFalse = 2
            }
    , Monkey{ startingItems = [72, 97, 50]
            , operation = (+ 2)
            , test = \x -> mod x 19 == 0
            , testNumber = 19
            , outputTrue = 6
            , outputFalse = 5
            }
    , Monkey{ startingItems = [90, 84, 56, 92, 91, 91]
            , operation = (* 19)
            , test = \x -> mod x 3 == 0
            , testNumber = 3
            , outputTrue = 6
            , outputFalse = 1
            }
    , Monkey{ startingItems = [63, 93, 55, 53]
            , operation = \x -> x * x
            , test = \x -> mod x 5 == 0
            , testNumber = 5
            , outputTrue = 3
            , outputFalse = 1
            }
    , Monkey{ startingItems = [50, 61, 52, 58, 86, 68, 97]
            , operation = (+ 4)
            , test = \x -> mod x 11 == 0
            , testNumber = 11
            , outputTrue = 5
            , outputFalse = 4
            }
    ]

operations1 =
    [ Monkey{ startingItems = [79, 98]
            , operation = (* 19)
            , test = \x -> mod x 23 == 0
            , testNumber = 23
            , outputTrue = 2
            , outputFalse = 3
            }
    , Monkey{ startingItems = [54, 65, 75, 74]
            , operation = (+ 6)
            , test = \x -> mod x 19 == 0
            , testNumber = 19
            , outputTrue = 2
            , outputFalse = 0
            }
    , Monkey{ startingItems = [79, 60, 97]
            , operation = \x -> x * x
            , test = \x -> mod x 13 == 0
            , testNumber = 13
            , outputTrue = 1
            , outputFalse = 3
            }
    , Monkey{ startingItems = [74]
            , operation = (+ 3)
            , test = \x -> mod x 17 == 0
            , testNumber = 17
            , outputTrue = 0
            , outputFalse = 1
            }
    ]
