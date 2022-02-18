module Main (main) where

import RIO
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.List.Split (chunksOf)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import FcCrypto.Glyph (Glyph, glyphBounds)
import FcCrypto.Glyph.Parser (glyphs)
import FcCrypto.Glyph.Diagram (drawGlyph)


readAndRender :: FilePath -> Int -> IO (Diagram B)
readAndRender fp k = do
  input <- readFileUtf8 fp
  case parseOnly (glyphs glyphBounds <* endOfInput) input of
    Left e -> fail e
    Right gs -> return $ drawGrid $ chunksOf k gs

drawGrid :: [[Glyph Bool]] -> Diagram B
drawGrid = vsep 1 . map (hsep 0.5 . map (frame 0.2 . alignT . drawGlyph))

main :: IO ()
main = mainWith readAndRender
