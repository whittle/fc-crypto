{-# LANGUAGE NoImplicitPrelude #-}

-- | Composed glyphs provided by game text.
module FcCrypto.Glyph.Known
  ( yo
  , co
  ) where

import RIO hiding (view)
import Control.Lens
import FcCrypto.Glyph


f :: Bool
f = False
t :: Bool
t = True

yo :: Glyph Bool
yo = ArrayGlyph
  (view (from verticals) [[f,f,f,f], [f,t,t,t], [t,f,f,f], [f,t,t,t], [f,t,f,f]])
  (view (from horizontals) [[f,f,f,f], [t,t,t,t], [f,f,f,t], [f,f,f,t], [t,t,f,f]])

co :: Glyph Bool
co = ArrayGlyph
  (view (from verticals) [[f,t,f,f], [f,t,t,f], [f,f,f,f], [f,t,t,t], [t,t,f,f]])
  (view (from horizontals) [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,f,f,t], [f,f,f,f]])
