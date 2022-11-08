module FcCrypto.Symbol.Diagram
  ( drawSymbol
  ) where

import RIO ()
import Diagrams
import Diagrams.Backend.SVG
import FcCrypto.Diagram (addLabel)
import FcCrypto.Glyph.Diagram (drawGlyph)
import FcCrypto.Symbol (sMeaning, Symbol(..))


drawSymbol :: Symbol Maybe Bool -> Diagram B
drawSymbol (Symbol mm g) = addLabel (sMeaning <$> mm) $ drawGlyph g
