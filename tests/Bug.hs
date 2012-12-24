{-# LANGUAGE NoImplicitPrelude #-}
module Bug where

import Bugs.B
import Bugs.C

main :: Fay ()
main = do
    foo "OK"
    bar "OK"
    mu "OK"
    zot "OK"
