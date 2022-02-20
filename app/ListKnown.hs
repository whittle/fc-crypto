{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO
import Diagrams.Backend.CmdLine (mainWith)
import FcCrypto.Commands (listKnown)


main :: IO ()
main = mainWith listKnown
