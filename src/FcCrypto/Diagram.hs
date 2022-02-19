{-# LANGUAGE NoImplicitPrelude #-}

-- | Drawing utilities not directly related to glyphs.
module FcCrypto.Diagram
  ( drawGrid
  , labelGlyph
  ) where


import RIO
import Data.List.Split (chunksOf)
import Diagrams
import Diagrams.Backend.SVG (B)
import qualified RIO.Text as T


-- Aesthetic constants

-- | Padding added around grid
border :: Double
border = 0.1

-- | Horizontal separation between glyphs
kerning :: Double
kerning = 0.5

-- | Vertical separation between rows of glyphs
leading :: Double
leading = 1.2


-- | Sized for laying out labeled glyphs in a grid. First argument is the width
-- of the grid in subdiagrams.
drawGrid :: Int -> [Diagram B] -> Diagram B
drawGrid k = frame border . vsep leading . map (hsep kerning) . chunksOf k . map alignT

-- | Add a possible label beneath a diagram.
labelGlyph :: Maybe Text -> Diagram B -> Diagram B
labelGlyph Nothing g = g
labelGlyph (Just l) g = g === alignedText 0.5 1 (T.unpack l)
