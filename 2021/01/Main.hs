module Main where

import System.IO
import Control.Monad
import Text.Read
import Data.Foldable

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- hGetContents $ handle
    
    let inputs = map read $ words content :: [Int]
    let increase = increase' inputs
    print $ sum increase

    hClose handle

increase' [] = []
increase' [a] = []
increase' (a:b:xs) = (if a < b then 1 else 0) : increase' (b:xs)