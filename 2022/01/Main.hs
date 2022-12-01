{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    content <- lines <$> hGetContents handle

    let (_, output) = count content (0, [])

    print $ maximum output

    hClose handle


count :: [String] -> (Int, [Int]) -> (Int, [Int])
count [] (a, as) = (0, a :as)
count ("":xs) (n, ns) = count xs (0, n : ns)
count (a:as) (n, ns) = count as (n + read a, ns)
