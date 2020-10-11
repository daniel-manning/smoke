module Problems where

import Lib 

eulerProblem1 = Problem  (Sum (Set Naturals [Filter "(n `mod` 3 == 0 || n `mod` 5 == 0)"])) 1000  "/home/dan/Desktop/euler/001.hs"