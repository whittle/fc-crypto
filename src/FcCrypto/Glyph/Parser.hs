{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module FcCrypto.Glyph.Parser
  ( glyphs
  , glyph
  , termLine
  , vertLine
  , horzLine
  , hrule
  , vrule
  ) where

import RIO
import Data.Array
import Data.Attoparsec.Text
import qualified RIO.List as L
import FcCrypto.Glyph


glyphs :: (GlyphIx, GlyphIx) -> Parser [Glyph Bool]
glyphs bs = glyph bs `sepBy` char '\n'

glyph :: (GlyphIx, GlyphIx) -> Parser (Glyph Bool)
glyph bs = (".\n" *> pure PeriodGlyph) <|> arrayGlyph bs

arrayGlyph :: (GlyphIx, GlyphIx) -> Parser (Glyph Bool)
arrayGlyph bs@((i1, j1), (i2, j2)) = do
  let hAssocs k = assocs . ixmap ((k,j1),(k,j2)) snd
      vAssocs k = assocs . ixmap ((i1,k),(i2,k)) fst
  -- FIXME: these should be based on the array indices
  l1 <- hAssocs 0 <$> termLine (j1,j2)
  l2 <- vAssocs 0 <$> vertLine (i1,i2)
  l3 <- hAssocs 1 <$> horzLine (j1,j2)
  l4 <- vAssocs 1 <$> vertLine (i1,i2)
  l5 <- hAssocs 2 <$> horzLine (j1,j2)
  l6 <- vAssocs 2 <$> vertLine (i1,i2)
  l7 <- hAssocs 3 <$> horzLine (j1,j2)
  l8 <- vAssocs 3 <$> vertLine (i1,i2)
  l9 <- hAssocs 4 <$> termLine (j1,j2)
  pure $ ArrayGlyph
    (Vertical $ array bs $ concat [l2, l4, l6, l8])
    (Horizontal $ array bs $ concat [l1, l3, l5, l7, l9])


type Line = Array Word8 Bool

termLine :: GlyphIx -> Parser Line
termLine b = fmap (listArray b) $
  char '*' *> (hrule `sepBy` (notChar '\n')) <* char '*' <* endOfLine

vertLine :: GlyphIx -> Parser Line
vertLine b = fmap (listArray b . (<> L.repeat False)) $
  vrule `sepBy` "  " <* endOfLine

horzLine :: GlyphIx -> Parser Line
horzLine b = fmap (listArray b . (<> L.repeat False)) $
  notChar '\n' *> (hrule `sepBy` (notChar '\n')) <* manyTill (notChar '\n') endOfLine

hrule :: Parser Bool
hrule = ("--" *> pure True)
    <|> ("  " *> pure False)

vrule :: Parser Bool
vrule = (char '|' *> pure True)
    <|> (char ' ' *> pure False)
