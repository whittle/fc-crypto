{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module FcCrypto.Symbol
  ( Symbol(..)
  , Meaning(..)
  , sMeaning
  ) where

import RIO
import FcCrypto.Glyph (Glyph)


-- | Whereas a @Glyph@ is a collection of line segments in a grid, a
-- symbol is a pairing of a glyph with its meaning. 
data Symbol f e = Symbol
  { symbolMeaning :: f Meaning
  , symbolGlyph :: Glyph e
  }

deriving stock instance (Eq (f Meaning), Eq e) => Eq (Symbol f e)
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


sMeaning :: Meaning -> String
sMeaning (Simple a) = [a]
sMeaning (Composed a1 a2) = [a1, a2]
sMeaning Period = "."
