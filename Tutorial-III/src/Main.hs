module Main where

import Tut3

tptest = [[1,2,3],[4,5,6],[7,8,9]]

main = 
  do putStrLn "Tutorial 3"
     let tpout = tpose tptest
     putStrLn (show tpout)
     
