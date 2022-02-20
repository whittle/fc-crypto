{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO
import Diagrams.Backend.CmdLine (mainWith)
import FcCrypto.Commands (renderMessage)


main :: IO ()
main = mainWith renderMessage
