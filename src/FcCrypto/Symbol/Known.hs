{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Composed symbols provided by game text.
module FcCrypto.Symbol.Known
  ( identify
  , all
  , period
  , yo, uh, av, df, co, le, ho, ed
  , ew, or, it, nm, rw, ag, np, ta
  , m_, o_, r_, t_, i_, f_, e_
  ) where

import RIO hiding (all, or, view)
import qualified RIO.List as L
import Control.Lens
import FcCrypto.Glyph
import FcCrypto.Symbol


identify :: Glyph Bool -> Symbol Maybe Bool
identify g = maybe (Symbol Nothing g) pSymbol $ L.find matchGlyph all
  where
    pSymbol (Symbol (Identity m) g') = Symbol (Just m) g'
    matchGlyph (Symbol _ g') = g == g'

all :: [Symbol Identity Bool]
all =
  [ yo, uh, av, df, co, le, ho, ed
  , period
  , ew, or, it, nm, rw, ag, np, ta
  , m_, o_, r_, t_, i_, f_, e_
  ]

f :: Bool
f = False
t :: Bool
t = True

yo :: Symbol Identity Bool
yo = Symbol (Identity $ Composed 'Y' 'O') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,t,t,t], [t,f,f,f], [f,t,t,t], [f,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,t], [f,f,f,t], [f,f,f,t], [t,t,f,f]])

uh :: Symbol Identity Bool
uh = Symbol (Identity $ Composed 'U' 'H') $ ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,t,t,f], [f,t,t,f], [f,f,f,f], [f,t,t,t]])
  (from horizontals `view` [[f,f,f,f], [t,t,f,f], [t,t,t,t], [f,t,t,f], [f,f,f,t]])

av :: Symbol Identity Bool
av = Symbol (Identity $ Composed 'A' 'V') $ ArrayGlyph
  (from verticals `view` [[f,f,t,t], [f,t,t,t], [f,t,t,f], [f,f,f,t], [f,t,t,t]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [f,f,f,t], [t,t,t,f], [f,f,f,t]])

df :: Symbol Identity Bool
df = Symbol (Identity $ Composed 'D' 'F') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,t], [f,t,t,t], [f,t,t,t], [f,t,t,f]])
  (from horizontals `view` [[f,f,f,f], [f,f,f,t], [f,t,f,f], [f,t,t,t], [f,f,f,f]])

co :: Symbol Identity Bool
co = Symbol (Identity $ Composed 'C' 'O') $ ArrayGlyph
  (from verticals `view` [[f,t,f,f], [f,t,t,f], [f,f,f,f], [f,t,t,t], [t,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,f,f,t], [f,f,f,f]])

le :: Symbol Identity Bool
le = Symbol (Identity $ Composed 'L' 'E') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,t,f,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [t,t,f,t], [f,t,t,f], [f,f,f,f]])

ho :: Symbol Identity Bool
ho = Symbol (Identity $ Composed 'H' 'O') $ ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,f,f,f], [f,f,f,f], [f,t,t,t], [f,t,t,t]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [t,t,t,t], [f,f,f,t], [f,f,f,t]])

ed :: Symbol Identity Bool
ed = Symbol (Identity $ Composed 'E' 'D') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,t], [t,t,t,t], [f,t,t,f], [f,t,t,f]])
  (from horizontals `view` [[f,f,f,f], [f,t,f,t], [t,t,f,f], [f,t,t,f], [f,f,f,f]])

period :: Symbol Identity Bool
period = Symbol (Identity Period) PeriodGlyph

ew :: Symbol Identity Bool
ew = Symbol (Identity $ Composed 'E' 'W') $ ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,f,t,f], [t,t,t,t], [f,f,t,f], [t,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [f,t,f,f], [t,t,f,t], [f,t,t,f], [f,f,f,f]])

or :: Symbol Identity Bool
or = Symbol (Identity $ Composed 'O' 'R') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,f], [f,f,f,f], [t,t,t,t], [f,t,f,f]])
  (from horizontals `view` [[f,f,f,t], [t,t,t,f], [f,f,t,t], [f,t,t,t], [f,f,f,f]])

it :: Symbol Identity Bool
it = Symbol (Identity $ Composed 'I' 'T') $ ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,f,t,t], [f,t,t,t], [t,t,t,f], [t,f,f,f]])
  (from horizontals `view` [[f,f,t,t], [t,t,t,t], [f,f,f,f], [f,t,t,t], [f,f,t,t]])

nm :: Symbol Identity Bool
nm = Symbol (Identity $ Composed 'N' 'M') $ ArrayGlyph
  (from verticals `view` [[f,t,f,f], [f,t,t,t], [f,f,t,f], [f,t,t,t], [t,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,f,f,t], [f,t,t,t], [f,t,t,f], [t,f,f,f]])

rw :: Symbol Identity Bool
rw = Symbol (Identity $ Composed 'R' 'W') $ ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,f,t,f], [t,t,t,f], [t,t,t,f], [t,t,f,f]])
  (from horizontals `view` [[f,f,f,t], [f,f,t,f], [t,f,t,t], [f,t,t,f], [f,f,f,f]])

ag :: Symbol Identity Bool
ag = Symbol (Identity $ Composed 'A' 'G') $ ArrayGlyph
  (from verticals `view` [[t,t,t,t], [f,t,t,t], [f,f,f,f], [f,t,t,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [f,f,f,f], [t,t,t,f], [f,f,f,f]])

np :: Symbol Identity Bool
np = Symbol (Identity $ Composed 'N' 'P') $ ArrayGlyph
  (from verticals `view` [[f,f,t,f], [f,t,t,t], [t,t,t,f], [t,t,f,f], [t,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [f,f,t,f], [f,t,t,t], [t,t,f,f], [t,f,f,f]])

ta :: Symbol Identity Bool
ta = Symbol (Identity $ Composed 'T' 'A') $ ArrayGlyph
  (from verticals `view` [[t,t,t,t], [f,t,t,t], [f,t,t,f], [f,f,f,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [f,f,f,f], [t,t,t,t], [f,f,f,f]])

m_ :: Symbol Identity Bool
m_ = Symbol (Identity $ Simple 'M') $ ArrayGlyph
  (from verticals `view` [[f,t,f,f], [f,t,t,f], [f,f,t,f], [f,t,t,t], [f,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,f,f,t], [f,t,f,f], [f,f,t,f], [f,f,f,f]])

o_ :: Symbol Identity Bool
o_ = Symbol (Identity $ Simple 'O') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,f], [f,f,f,f], [f,t,t,t], [f,t,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [f,f,f,t], [f,f,f,t], [f,f,f,f]])

r_ :: Symbol Identity Bool
r_ = Symbol (Identity $ Simple 'R') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,f], [f,f,f,f], [t,t,t,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,t], [f,f,t,f], [f,f,t,t], [f,t,t,f], [f,f,f,f]])

t_ :: Symbol Identity Bool
t_ = Symbol (Identity $ Simple 'T') $ ArrayGlyph
  (from verticals `view` [[t,t,f,f], [f,f,t,t], [f,t,t,f], [f,f,f,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [t,t,t,f], [f,f,f,f], [f,t,t,t], [f,f,f,f]])

i_ :: Symbol Identity Bool
i_ = Symbol (Identity $ Simple 'I') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,f], [f,f,f,t], [t,t,t,f], [t,f,f,f]])
  (from horizontals `view` [[f,f,t,t], [f,f,t,t], [f,f,f,f], [f,f,t,t], [f,f,t,t]])

f_ :: Symbol Identity Bool
f_ = Symbol (Identity $ Simple 'F') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,f], [f,t,t,t], [f,f,f,t], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [f,f,f,f], [f,t,f,f], [f,t,t,t], [f,f,f,f]])

e_ :: Symbol Identity Bool
e_ = Symbol (Identity $ Simple 'E') $ ArrayGlyph
  (from verticals `view` [[f,f,f,f], [f,f,f,f], [t,t,t,t], [f,f,f,f], [f,f,f,f]])
  (from horizontals `view` [[f,f,f,f], [f,t,f,f], [t,t,f,f], [f,f,t,f], [f,f,f,f]])
