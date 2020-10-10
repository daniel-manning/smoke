module Main where

import Lib

main :: IO ()
main = writeFile "/home/dan/Desktop/euler/001.hs" (unlines programme)

programme = ["sumSet k = sum $ takeWhile (<k) [n | n <- [1..], (n `mod` 3 == 0 || n `mod` 5 == 0)]", "main = print $ sumSet 1000"]
