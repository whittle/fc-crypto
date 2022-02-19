{-# LANGUAGE LambdaCase #-}

module Main (main) where

import RIO
import Diagrams
import Diagrams.Backend.SVG.CmdLine
import FcCrypto.Diagram (addLabel, drawGrid)
import FcCrypto.Glyph (Glyph)
import FcCrypto.Glyph.Diagram (drawGlyph)
import qualified FcCrypto.Glyph.Known as Known
import FcCrypto.Glyph.Parser (parseOnlyGlyphs)


main :: IO ()
main = mainWith $ \case
  "original" -> original
  "labelKnown" -> labelKnown
  _ -> error "choose one of: original, listKnown, labelKnown"

-- | Render the original message with no attempt to solve it.
original :: FilePath -> Int -> IO (Diagram B)
original fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  return $ drawGrid k $ map drawGlyph gs

-- | Render the original message with all known glyphs labeled.
labelKnown :: FilePath -> Int -> IO (Diagram B)
labelKnown fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  let lgs = (id &&& Known.identify) <$> gs
  return $ drawGrid k $ map drawLabeledGlyph lgs


-- Helpers

drawLabeledGlyph :: (Glyph Bool, Maybe Text) -> Diagram B
drawLabeledGlyph (g, l) = addLabel l $ drawGlyph g
