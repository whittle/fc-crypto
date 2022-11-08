{-# LANGUAGE NoImplicitPrelude #-}

-- | Drawing utilities not directly related to glyphs.
module FcCrypto.Diagram
  ( addLabel
  , drawGrid
  ) where


import RIO
import Data.List.Split (chunksOf)
import Diagrams
import Diagrams.Backend.SVG (B)


-- Aesthetic constants

-- | Padding added around grid
border :: Double
border = 0.1

-- | Horizontal separation between glyphs
kerning :: Double
kerning = 0.5

-- | Vertical separation between rows of glyphs
leading :: Double
leading = 0.5


-- | Sized for laying out labeled glyphs in a grid. First argument is the width
-- of the grid in subdiagrams.
drawGrid :: Int -> [Diagram B] -> Diagram B
drawGrid k = frame border . vsep leading . map (hsep kerning) . chunksOf k . map alignT

-- | Add a possible label below a diagram. Does not add space below a diagram if
-- no label is present.
addLabel :: Maybe String -> Diagram B -> Diagram B
addLabel Nothing = id
addLabel (Just l) = extrudeEnvelope unit_Y . (=== alignedText 0.5 1 l)
