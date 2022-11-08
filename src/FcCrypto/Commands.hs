{-# LANGUAGE NoImplicitPrelude #-}

-- | The commands that get executed by the different executables.
module FcCrypto.Commands
  ( labelKnown
  , listAlphabet
  , listKnown
  , renderMessage
  ) where

import RIO
import Diagrams
import Diagrams.Backend.SVG.CmdLine (B)
import FcCrypto.Diagram (drawGrid)
import FcCrypto.Glyph.Diagram (drawGlyph)
import FcCrypto.Symbol (Symbol(..))
import qualified FcCrypto.Symbol as Symbol
import FcCrypto.Symbol.Diagram (drawSymbol)
import qualified FcCrypto.Symbol.Known as Known
import FcCrypto.Glyph.Parser (parseOnlyGlyphs)


-- | Render the original message with all known glyphs labeled.
labelKnown :: FilePath -> Int -> IO (Diagram B)
labelKnown fp k = do
  input <- readFileUtf8 fp
  gs <- either fail pure $ parseOnlyGlyphs input
  pure $ drawGrid k $ map drawSymbol $ Known.identify <$> gs

-- | Render known and suspected simple glyphs.
listAlphabet :: Int -> IO (Diagram B)
listAlphabet gridSize = do
  let a = jus $ Symbol.intersection (Symbol.intersection Known.av Known.ag) Known.ta
      c = jus $ Symbol.difference Known.co Known.o_
      d = jus $ Symbol.intersection Known.df Known.ed
      e = jus Known.e_
      f = jus Known.f_
      h' = Symbol.intersection Known.uh Known.ho
      i = jus Known.i_
      l = jus $ Symbol.difference Known.le Known.e_
      m = jus Known.m_
      n' = Symbol.intersection Known.nm Known.np
      o = jus Known.o_
      p = jus $ Symbol.difference Known.np n'
      r = jus Known.r_
      t = jus Known.t_
      u = jus $ Symbol.difference Known.uh h'
      w = jus $ Symbol.intersection Known.ew Known.rw
      y = jus $ Symbol.difference Known.yo Known.o_
  pure $ drawGrid gridSize $ map drawSymbol
    [a,c,d,e,f,jus h',i,l,m,jus n',o,p,r,t,u,w,y]
  where jus (Symbol (Identity m) g) = Symbol (Just m) g

-- | Render the known glyphs with their labels.
listKnown :: Int -> IO (Diagram B)
listKnown k = pure $ drawGrid k $ map (drawSymbol . f) Known.all
  where f (Symbol (Identity m) g) = Symbol (Just m) g

-- | Render the original message with no attempt to solve it.
renderMessage :: FilePath -> Int -> IO (Diagram B)
renderMessage fp k = do
  input <- readFileUtf8 fp
  gs <- either fail return $ parseOnlyGlyphs input
  pure $ drawGrid k $ map drawGlyph gs
