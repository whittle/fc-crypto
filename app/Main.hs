module Main (main) where

import RIO
import Diagrams
import Diagrams.Backend.SVG.CmdLine
import FcCrypto.Diagram (drawGrid, labelGlyph)
import FcCrypto.Glyph (Glyph)
import FcCrypto.Glyph.Diagram (drawGlyph)
import qualified FcCrypto.Glyph.Known as Known
import FcCrypto.Glyph.Parser (parseOnlyGlyphs)


main :: IO ()
main = mainWith process

process :: FilePath -> Int -> IO (Diagram B)
process fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  let lgs = (id &&& Known.identify) <$> gs
  return $ drawGrid k $ map drawLabeledGlyph lgs

drawLabeledGlyph :: (Glyph Bool, Maybe Text) -> Diagram B
drawLabeledGlyph (g, l) = labelGlyph l $ drawGlyph g
