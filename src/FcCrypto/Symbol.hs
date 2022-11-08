{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module FcCrypto.Symbol
  ( Symbol(..)
  , Meaning(..)
  , sMeaning
  , difference
  , intersection
  ) where

import RIO
import FcCrypto.Glyph (Glyph)
import qualified FcCrypto.Glyph as Glyph


-- | Whereas a @Glyph@ is a collection of line segments in a grid, a
-- symbol is a pairing of a glyph with its meaning. 
data Symbol f e = Symbol
  { symbolMeaning :: f Meaning
  , symbolGlyph :: Glyph e
  }

deriving stock instance (Eq (f Meaning), Eq e) => Eq (Symbol f e)
deriving stock instance Functor (Symbol f)
deriving stock instance (Show (f Meaning), Show e) => Show (Symbol f e)


-- | If a symbol is “simple,” then it corresponds to a single English
-- letter. If it’s “composed” then its meaning is two English letters
-- (in no particular order). If a glyph is a period, it’s a period and
-- it goes with a period glyph.
data Meaning
  = Simple Char
  | Composed Char Char
  | Period
  deriving stock (Show)

instance Eq Meaning where
  Simple a == Simple b = a == b
  Composed a1 a2 == Composed b1 b2 =
    ((a1 == b1) && (a2 == b2)) || ((a1 == b2) && (a2 == b1))
  Period == Period = True
  _ == _ = False

meaningDifference :: Meaning -> Meaning -> Meaning
meaningDifference (Composed a1 a2) (Simple b)
  | a1 == b = Simple a2
  | a2 == b = Simple a1
  | otherwise = Period
meaningDifference _ _ = Period

meaningIntersection :: Meaning -> Meaning -> Meaning
meaningIntersection (Simple a) (Simple b) =
  if a == b then Simple a else Period
meaningIntersection (Simple a) (Composed b1 b2) =
  if a == b1 || a == b2 then Simple a else Period
meaningIntersection (Composed a1 a2) (Simple b) =
  if a1 == b || a2 == b then Simple b else Period
meaningIntersection (Composed a1 a2) (Composed b1 b2)
  | a1 == b1 || a1 == b2 = Simple a1
  | a2 == b1 || a2 == b2 = Simple a2
  | otherwise = Period
meaningIntersection _ _ = Period


sMeaning :: Meaning -> String
sMeaning (Simple a) = [a]
sMeaning (Composed a1 a2) = [a1, a2]
sMeaning Period = "."

difference :: Symbol Identity Bool -> Symbol Identity Bool -> Symbol Identity Bool
difference (Symbol (Identity m1) g1) (Symbol (Identity m2) g2) =
  Symbol (Identity $ meaningDifference m1 m2) $ Glyph.difference g1 g2

intersection :: Symbol Identity Bool -> Symbol Identity Bool -> Symbol Identity Bool
intersection (Symbol (Identity m1) g1) (Symbol (Identity m2) g2) = 
  Symbol (Identity $ meaningIntersection m1 m2) $ Glyph.intersection g1 g2
