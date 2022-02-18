{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |

module FcCrypto.Glyph.Diagram
  ( drawGlyph
  ) where

import RIO
import qualified FcCrypto.Glyph as G
import Diagrams.Prelude hiding (view)
import Diagrams.Backend.SVG
import qualified RIO.List as L


corner :: Diagram B
corner = circle 0.1 # fc black

corners :: Diagram B
corners = atPoints (trailVertices $ square 2) (L.repeat corner)

period :: Diagram B
period = circle 0.2 # fc black

layoutV :: G.Segments 'G.Vert Bool -> Diagram B
layoutV = translate (r2 (-1,1)) . hsep 0.5 . map (alignT . vcat . map f) . view G.verticals
  where f b = if b then vr else strutY 0.5
        vr = vrule 0.5 # lw thick # lineCap LineCapRound

layoutH :: G.Segments 'G.Horz Bool -> Diagram B
layoutH = translate (r2 (-1,1)) . vsep 0.5 . map (alignL . hcat . map f) . view G.horizontals
  where f b = if b then hr else strutX 0.5
        hr = hrule 0.5 # lw thick # lineCap LineCapRound

drawGlyph :: G.Glyph Bool -> Diagram B
drawGlyph G.PeriodGlyph = alignB period `atop` alignB corners
drawGlyph (G.ArrayGlyph v h) = layoutH h `atop` layoutV v `atop` corners
