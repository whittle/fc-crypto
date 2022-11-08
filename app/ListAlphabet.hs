{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO
import Diagrams.Backend.CmdLine (mainWith)
import FcCrypto.Commands (listAlphabet)


main :: IO ()
main = mainWith listAlphabet
