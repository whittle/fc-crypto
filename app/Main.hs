{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO
import Diagrams.Backend.CmdLine (mainWith)
import FcCrypto.Commands (labelKnown)


main :: IO ()
main = mainWith labelKnown
