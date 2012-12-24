{-# LANGUAGE NoImplicitPrelude #-}
module Bugs.A where

import Language.Fay.Prelude

foo :: Int -> Fay ()
foo _ = putStrLn "WRONG!"

zot x = putStrLn ("A.hs:zot: " ++ x)
