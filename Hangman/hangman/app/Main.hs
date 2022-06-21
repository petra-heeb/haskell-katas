module Main where

import Hangman

main :: IO ()
main = do
    putStrLn "Think of a word:"
    word <- getLine
    play word