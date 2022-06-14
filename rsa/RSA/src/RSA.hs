module RSA
    ( encrypt, decrypt, computeBigN, computeSmallN, chooseC
    ) where

import System.Random -- for chooseC function
import Data.ByteString

encrypt :: String -> Int -> Int -> (String, (Int, Int)) -- return: (Message, (N, c)) where (N, c) is the public key
encrypt message p q = (message, (computeBigN p q, chooseC p q))

computeBigN :: Int -> Int -> Int
computeBigN p q = p * q

computeSmallN :: Int -> Int -> Int
computeSmallN p q = (p - 1) * (q - 1)

chooseC :: Int -> Int -> 
chooseC p q = computeSmallN - 10
-- chooseC p q = randomRIO (1, computeSmallN p q)

determineD :: Int -> Int -> Int
determineD c n = (^) c -1 `mod` n

sliceMessageToThreeBytes :: String -> [String]
sliceMessageToThreeBytes string = take 3 $ fromString string : sliceMessageToThreeBytes string

sliceMessageToFourBytes :: String -> [String]
sliceMessageToFourBytes = undefined

decrypt = undefined