{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Composed glyphs provided by game text.
module FcCrypto.Glyph.Known
  ( identify
  , all
  , yo
  , uh
  , av
  , df
  , co
  , le
  , ho
  , ed
  ) where

import RIO hiding (all, view)
import Control.Lens
import FcCrypto.Glyph


identify :: Glyph Bool -> Maybe Text
identify = flip lookup all

all :: [(Glyph Bool, Text)]
all =
  [ (yo, "YO")
  , (uh, "UH")
  , (av, "AV")
  , (df, "DF")
  , (co, "CO")
  , (le, "LE")
  , (ho, "HO")
  , (ed, "ED")
  ]

f :: Bool
f = False
t :: Bool
t = True

yo :: Glyph Bool
yo = ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,t,t,t], [t,f,f,f], [f,t,t,t], [f,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,t], [f,f,f,t], [f,f,f,t], [t,t,f,f]])

uh :: Glyph Bool
uh = ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,t,t,f], [f,t,t,f], [f,f,f,f], [f,t,t,t]])
  (from horizontals `view` [[f,f,f,f], [t,t,f,f], [t,t,t,t], [f,t,t,f], [f,f,f,t]])

av :: Glyph Bool
av = ArrayGlyph
  (from verticals `view` [[f,f,t,t], [f,t,t,t], [f,t,t,f], [f,f,f,t], [f,t,t,t]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [f,f,f,t], [t,t,t,f], [f,f,f,t]])

df :: Glyph Bool
df = ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,t], [f,t,t,t], [f,t,t,t], [f,t,t,f]])
  (from horizontals `view` [[f,f,f,f], [f,f,f,t], [f,t,f,f], [f,f,f,t], [f,f,f,f]])

co :: Glyph Bool
co = ArrayGlyph
  (from verticals `view` [[f,t,f,f], [f,t,t,f], [f,f,f,f], [f,t,t,t], [t,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,f,f,t], [f,f,f,f]])

le :: Glyph Bool
le = ArrayGlyph
  (from verticals `view` [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,t,f,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [t,t,f,t], [f,t,t,f], [f,f,f,f]])

ho :: Glyph Bool
ho = ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,f,f,f], [f,f,f,f], [f,t,t,t], [f,t,t,t]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,f,f,t], [f,f,f,t]])

ed :: Glyph Bool
ed = ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,t], [t,t,t,t], [f,t,t,f], [f,t,t,f]])
  (from horizontals `view` [[f,f,f,f], [f,t,f,t], [t,t,f,f], [f,t,t,f], [f,f,f,f]])
