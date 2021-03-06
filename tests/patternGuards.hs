{-# LANGUAGE FlexibleInstances #-}

-- | As pattern matches

import           Prelude
import           FFI

isPositive :: Double -> Bool
isPositive x | x > 0 = True
             | x <= 0 = False

threeConds x | x > 1 = 2
             | x == 1 = 1
             | x < 1 = 0

withOtherwise x | x > 1 = True
                | otherwise = False

-- Not called, throws "non-exhaustive guard"
nonExhaustive x | x > 1 = True

main :: Fay ()
main = do
  putStrLn $ showListB [isPositive 1, isPositive 0]
  putStrLn $ showListD [threeConds 3, threeConds 1, threeConds 0]
  putStrLn $ showListB [withOtherwise 2, withOtherwise 0]

showListB :: [Bool] -> String
showListB = ffi "JSON.stringify(%1)"

showListD :: [Double] -> String
showListD = ffi "JSON.stringify(%1)"
