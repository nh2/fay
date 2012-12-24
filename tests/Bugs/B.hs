{-# LANGUAGE NoImplicitPrelude #-}
module Bugs.B where

import Bugs.A
import Language.Fay.Prelude

bar x = putStrLn ("B.hs:bar: " ++ x)

mu = zot
