{-# LANGUAGE NoImplicitPrelude #-}
module Bug where

import Bugs.B
import Bugs.C

same x = putStrLn ("Bug.hs:same: " ++ x)

main :: Fay ()
main = do
    foo "OK"
    bar "OK"
    mu "OK"
    zot "OK"
    same "OK"
