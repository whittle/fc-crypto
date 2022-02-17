{-# LANGUAGE OverloadedStrings #-}

module FcCrypto.Glyph.ParserTest where

import RIO
import Data.Array (listArray)
import Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified FcCrypto.Glyph.Known as Known
import FcCrypto.Glyph (glyphBounds)
import FcCrypto.Glyph.Parser
import Test.Tasty.HUnit


unit_parseYo :: IO ()
unit_parseYo = parseOnly (glyph glyphBounds) yoText @?= Right Known.yo

unit_parseCo :: IO ()
unit_parseCo = parseOnly (glyph glyphBounds) coText @?= Right Known.co

unit_parseYoCo :: IO ()
unit_parseYoCo = parseOnly (glyphs glyphBounds) (yoText<>"\n"<>coText)
             @?= Right [Known.yo, Known.co]

unit_parse1 :: IO ()
unit_parse1 = parseOnly (termLine (0,3) <* endOfInput) "*           *\n"
          @?= Right (listArray (0,3) [False,False,False,False])

unit_parse2 :: IO ()
unit_parse2 = parseOnly (vertLine (0,4) <* endOfInput) "      |\n"
          @?= Right (listArray (0,4) [False,False,True,False,False])

unit_parse3 :: IO ()
unit_parse3 = parseOnly (horzLine (0,3) <* endOfInput) " --+--+--+--+\n"
          @?= Right (listArray (0,3) [True,True,True,True])

unit_parse4 :: IO ()
unit_parse4 = parseOnly (vertLine (0,4) <* endOfInput) "   |     |  |\n"
          @?= Right (listArray (0,4) [False,True,False,True,True])

unit_parse5 :: IO ()
unit_parse5 = parseOnly (horzLine (0,3) <* endOfInput) "   |     +--+\n"
          @?= Right (listArray (0,3) [False,False,False,True])

unit_parse6 :: IO ()
unit_parse6 = parseOnly (vertLine (0,4) <* endOfInput) "   |     |\n"
          @?= Right (listArray (0,4) [False,True,False,True,False])

unit_parse7 :: IO ()
unit_parse7 = parseOnly (horzLine (0,3) <* endOfInput) "   |     +--\n"
          @?= Right (listArray (0,3) [False,False,False,True])

unit_parse9 :: IO ()
unit_parse9 = parseOnly (termLine (0,3) <* endOfInput) "*--+--      *\n"
          @?= Right (listArray (0,3) [True,True,False,False])

unit_parseX :: IO ()
unit_parseX = parseOnly (horzLine (0,3) <* endOfInput) "   +--+\n"
          @?= Right (listArray (0,3) [False,True,False,False])

unit_parseY :: IO ()
unit_parseY = parseOnly (horzLine (0,3) <* endOfInput) "+--+--\n"
          @?= Right (listArray (0,3) [True,True,False,False])

yoText :: Text
yoText = "*           *\n\
         \      |      \n\
         \ --+--+--+--+\n\
         \   |     |  |\n\
         \   |     +--+\n\
         \   |     |   \n\
         \   |     +-- \n\
         \   |     |   \n\
         \*--+--      *\n"

coText :: Text
coText = "*           *\n\
         \            |\n\
         \+--+-----+  |\n\
         \|  |     |  |\n\
         \+--+--+--+--+\n\
         \   |     |   \n\
         \         +-- \n\
         \         |   \n\
         \*           *\n"
