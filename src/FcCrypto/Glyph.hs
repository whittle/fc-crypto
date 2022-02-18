{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module FcCrypto.Glyph
  ( GlyphIx
  , glyphBounds
  , Orient(..)
  , Directed(..)
  , Segments
  , verticals
  , horizontals
  , Glyph(..)
  ) where

import RIO
import Control.Lens
import Data.Array
import Data.Kind (Type)
import qualified RIO.List as L


type GlyphIx = (Word8, Word8)

-- | Slight misnomer: these are the bounds for each of the arrays in the glyph.
-- The positions within a glyph are difficult to assign to a single
-- undifferentiated rectangular array.
glyphBounds :: (GlyphIx, GlyphIx)
glyphBounds = ((0,0),(4,3))


-- | Type-level discriminator for vertical and horizontal glyph components.
data Orient = Vert | Horz
  deriving stock (Eq, Show)

-- | Because the vertical and horizaontal elements of glyphs get stored in
-- identically-sized arrays, @Directed@ provides a way to discriminate between
-- them at the type and value levels.
data Directed :: Orient -> Type -> Type where
  Vertical :: a -> Directed 'Vert a
  Horizontal :: a -> Directed 'Horz a

deriving stock instance Eq a => Eq (Directed o a)
deriving stock instance Show a => Show (Directed o a)


type Segments o a = Directed o (Array GlyphIx a)


verticals :: Iso' (Segments 'Vert a) [[a]]
verticals = iso (\(Vertical a) -> arrayToLists a)
                (Vertical . listArray glyphBounds . concat)

horizontals :: Iso' (Segments 'Horz a) [[a]]
horizontals = iso (\(Horizontal a) -> arrayToLists a)
                  (Horizontal . listArray glyphBounds . concat)


{- | Represents all of the line positions in a single glyph. The possible values
   present at those positions are parameterized.

   Possible vertical lines in a glyph:
    . ----- ----- ----- ----- .
   (0,0) (1,0) (2,0) (3,0) (4,0)
      ----- ----- ----- -----
   (0,1) (1,1) (2,1) (3,1) (4,1)
      ----- ----- ----- -----
   (0,2) (1,2) (2,2) (3,2) (4,2)
      ----- ----- ----- -----
   (0,3) (1,3) (2,3) (3,3) (4,3)
    . ----- ----- ----- ----- .

   Possible horizontal lines in a glyph:
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
data Glyph e
  = PeriodGlyph
  | ArrayGlyph (Segments 'Vert e) (Segments 'Horz e)
  deriving stock (Eq, Show)

arrayToLists :: Array GlyphIx e -> [[e]]
arrayToLists a = L.unfoldr go $ elems a
  where go es = case L.genericSplitAt (fst $ snd $ bounds a) es of
          ([], _) -> Nothing
          (is, xs) -> Just (is, xs)
