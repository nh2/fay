{-# LANGUAGE NoImplicitPrelude #-}
module Bugs.C where

import Language.Fay.Prelude

foo :: String -> Fay ()
foo x = putStrLn ("C.hs:foo: " ++ x)

zot x = putStrLn ("C.hs:zot: " ++ x)
