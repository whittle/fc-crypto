{-# LANGUAGE NoImplicitPrelude #-}

-- | Composed glyphs provided by game text.
module FcCrypto.Glyph.Known
  ( yo
  , co
  , mkArray
  ) where

import RIO
import Data.Array
import FcCrypto.Glyph


f :: Bool
f = False
t :: Bool
t = True

mkArray :: [[e]] -> Array GlyphIx e
mkArray = listArray glyphBounds . concat

yo :: Glyph Bool
yo = Glyph
  { glyphVert = mkArray [[f,f,f,f], [f,t,t,t], [t,f,f,f], [f,t,t,t], [f,t,f,f]]
  , glyphHorz = mkArray [[f,f,f,f], [t,t,t,t], [f,f,f,t], [f,f,f,t], [t,t,f,f]]
  }

co :: Glyph Bool
co = Glyph
  { glyphVert = mkArray [[f,t,f,f], [f,t,t,f], [f,f,f,f], [f,t,t,t], [t,t,f,f]]
  , glyphHorz = mkArray [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,f,f,t], [f,f,f,f]]
  }
