module Main (main) where

import RIO
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import FcCrypto.Glyph (extractHorz, extractVert, Glyph(..))
import FcCrypto.Glyph.Diagram (drawGlyph)
import qualified FcCrypto.Glyph.Known as Known

main :: IO ()
main = mainWith $ drawGlyph Known.yo ||| strutX 0.5 ||| drawGlyph Known.co
