module Problems where

import Lib 

eulerProblem1 = Problem  (Sum (Set (LTR Naturals) [Filter "(n `mod` 3 == 0 || n `mod` 5 == 0)"])) 1000  "/home/dan/Desktop/euler/001.hs"
eulerProblem2 = Problem  (Sum (Set (LTER Fibonacci) [Filter "(n `mod` 2 == 0)"])) 4000000  "/home/dan/Desktop/euler/002.hs"