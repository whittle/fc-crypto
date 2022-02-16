{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |

module FcCrypto.Glyph.Diagram
  ( drawGlyph
  ) where

import RIO
import FcCrypto.Glyph (extractHorz, extractVert, Glyph(..))
import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified RIO.List as L

corner :: Diagram B
corner = circle 0.1 # fc black

corners :: Diagram B
corners = atPoints (trailVertices $ square 2) (L.repeat corner) # translate (r2 (1, -1))

layoutV :: Glyph Bool -> Diagram B
layoutV = hsep 0.5 . map (alignT . vcat . map f) . extractVert
  where f b = if b then vrule 0.5 # lw ultraThick else strutY 0.5

layoutH :: Glyph Bool -> Diagram B
layoutH = vsep 0.5 . map (alignL . hcat . map f) . extractHorz
  where f b = if b then hrule 0.5 # lw ultraThick else strutX 0.5

drawGlyph :: Glyph Bool -> Diagram B
drawGlyph = atop corners . liftM2 atop layoutV layoutH
