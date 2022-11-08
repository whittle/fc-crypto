{-# LANGUAGE NoImplicitPrelude #-}

-- | The commands that get executed by the different executables.
module FcCrypto.Commands
  ( labelKnown
  , listKnown
  , renderMessage
  ) where

import RIO
import Diagrams
import Diagrams.Backend.SVG.CmdLine (B)
import FcCrypto.Diagram (drawGrid)
import FcCrypto.Glyph.Diagram (drawGlyph)
import FcCrypto.Symbol (Symbol(..))
import FcCrypto.Symbol.Diagram (drawSymbol)
import qualified FcCrypto.Symbol.Known as Known
import FcCrypto.Glyph.Parser (parseOnlyGlyphs)


-- | Render the original message with all known glyphs labeled.
labelKnown :: FilePath -> Int -> IO (Diagram B)
labelKnown fp k = do
  input <- readFileUtf8 fp
  gs <- either fail pure $ parseOnlyGlyphs input
  pure $ drawGrid k $ map drawSymbol $ Known.identify <$> gs

-- | Render the known glyphs with their labels.
listKnown :: Int -> IO (Diagram B)
listKnown k =
  pure $ drawGrid k $ map (drawSymbol . f) Known.all
  where f (Symbol (Identity m) g) = Symbol (Just m) g

-- | Render the original message with no attempt to solve it.
renderMessage :: FilePath -> Int -> IO (Diagram B)
renderMessage fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  pure $ drawGrid k $ map drawGlyph gs
