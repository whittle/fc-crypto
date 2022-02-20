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
import FcCrypto.Diagram (addLabel, drawGrid)
import FcCrypto.Glyph (Glyph)
import FcCrypto.Glyph.Diagram (drawGlyph)
import qualified FcCrypto.Glyph.Known as Known
import FcCrypto.Glyph.Parser (parseOnlyGlyphs)


-- | Render the original message with all known glyphs labeled.
labelKnown :: FilePath -> Int -> IO (Diagram B)
labelKnown fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  let lgs = (id &&& Known.identify) <$> gs
  return $ drawGrid k $ map drawLabeledGlyph lgs

-- | Render the known glyphs with their labels.
listKnown :: Int -> IO (Diagram B)
listKnown k =
  return $ drawGrid k $ map (drawLabeledGlyph . second Just) Known.all

-- | Render the original message with no attempt to solve it.
renderMessage :: FilePath -> Int -> IO (Diagram B)
renderMessage fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  return $ drawGrid k $ map drawGlyph gs


-- Helpers

drawLabeledGlyph :: (Glyph Bool, Maybe Text) -> Diagram B
drawLabeledGlyph (g, l) = addLabel l $ drawGlyph g
