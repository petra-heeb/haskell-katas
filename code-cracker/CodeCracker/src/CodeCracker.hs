module CodeCracker
    ( encrypt, decrypt
    ) where

import Data.List -- elemIndex function

alphabet = "abcdefghijklmnopqrstuvwxyz"
key = "!)\"(£*%&><@abcdefghijklmno"

-- crackCode :: String -> String
-- crackCode [] = []
-- crackCode (x:xs) = replaceCharacter x : crackCode xs

encrypt :: String -> String
encrypt [] = []
encrypt (x:xs) = replaceCharacter x alphabet key : encrypt xs

decrypt :: String -> String
decrypt [] = []
decrypt (x:xs) = replaceCharacter x key alphabet : decrypt xs

replaceCharacter :: Char -> [Char] -> [Char] -> Char
replaceCharacter c from to = to !! (fromJust $ elemIndex c from)

fromJust :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x