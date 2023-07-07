module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Lib.hs"
               , "src/Problem.hs"
               , "src/Answer.hs"
               , "src/Happiness.hs"
               , "src/Solver.hs"
               ]
