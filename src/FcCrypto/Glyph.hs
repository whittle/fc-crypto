{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module FcCrypto.Glyph
  ( GlyphIx
  , glyphBounds
  , Glyph(..)
  , arrayToLists
  , extractVert
  , extractHorz
  ) where

import RIO
import Data.Array
import qualified RIO.List as L


type GlyphIx = (Word8, Word8)

-- | Slight misnomer: these are the bounds for each of the arrays in the glyph.
-- The positions within a glyph are difficult to assign to a single
-- undifferentiated rectangular array.
glyphBounds :: (GlyphIx, GlyphIx)
glyphBounds = ((0,0),(4,3))

-- | Represents all of the line positions in a single glyph. The possible values
-- present at those positions are parameterized.
data Glyph e = Glyph
  {- | Possible vertical lines in a glyph
   . ----- ----- ----- ----- .
  (0,0) (1,0) (2,0) (3,0) (4,0)
     ----- ----- ----- -----
  (0,1) (1,1) (2,1) (3,1) (4,1)
     ----- ----- ----- -----
  (0,2) (1,2) (2,2) (3,2) (4,2)
     ----- ----- ----- -----
  (0,3) (1,3) (2,3) (3,3) (4,3)
   . ----- ----- ----- ----- .
  -}
  { glyphVert :: Array GlyphIx e
  {- | Possible horizontal lines in a glyph
  .(0,0) (0,1) (0,2) (0,3).
  |     |     |     |     |
   (1,0) (1,1) (1,2) (1,3)
  |     |     |     |     |
   (2,0) (2,1) (2,2) (2,3)
  |     |     |     |     |
   (3,0) (3,1) (3,2) (3,3)
  |     |     |     |     |
  .(4,0) (4,1) (4,2) (4,3).
  -}
  , glyphHorz :: Array GlyphIx e
  } deriving stock (Eq, Show)

arrayToLists :: GlyphIx -> Array i e -> [[e]]
arrayToLists (i, _) = L.unfoldr go . elems
  where go es = case L.genericSplitAt i es of
          ([], _) -> Nothing
          (is, xs) -> Just (is, xs)

extractVert :: Glyph a -> [[a]]
extractVert = arrayToLists (snd glyphBounds) . glyphVert

extractHorz :: Glyph a -> [[a]]
extractHorz = arrayToLists (snd glyphBounds) . glyphHorz
