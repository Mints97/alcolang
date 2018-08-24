module Main where

import Parser.Parser

main :: IO ()
main = print $ parseString "(param1 = int) (param2 = string) (+ param1 (* 2 param2)) "
