module Hangman ( play ) where

play :: String -> IO ()
play word = do
    putStrLn "Try to guess it:"
    guess <- getLine
    if guess == word then putStrLn "Yes you got it!" else do
        putStrLn (match word guess)
        play word

match :: String -> String -> String
match word guess = [if x `elem` guess then x else '_'| x <- word]