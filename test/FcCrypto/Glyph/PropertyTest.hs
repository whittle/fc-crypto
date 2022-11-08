{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FcCrypto.Glyph.PropertyTest where

import RIO
import Data.Array (listArray)
import FcCrypto.Glyph
import Test.Tasty.QuickCheck


prop_glyphFunctorIdentity :: ArbGlyph Bool -> Bool
prop_glyphFunctorIdentity (Gly a) = fmap id a == id a

prop_glyphFunctorComposition :: Fun Word8 String -> Fun Int Word8 -> ArbGlyph Int -> Bool
prop_glyphFunctorComposition (Fun _ f) (Fun _ g) (Gly a) =
  fmap (f . g) a == (fmap f . fmap g) a

prop_glyphApplicativeIdentity :: ArbGlyph Word -> Bool
prop_glyphApplicativeIdentity (Gly v) = (pure id <*> v) == v

prop_glyphApplicativeComposition :: ArbGlyph (Fun Char Word8)
                                 -> ArbGlyph (Fun Int Char)
                                 -> ArbGlyph Int
                                 -> Bool
prop_glyphApplicativeComposition (Gly u') (Gly v') (Gly w) =
  let u = applyFun <$> u' in
  let v = applyFun <$> v' in
  (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

prop_glyphApplicativeHomomorphism :: Fun Int Bool -> Int -> Bool
prop_glyphApplicativeHomomorphism (Fun _ f) x =
  (pure f <*> pure x) == (pure (f x) :: Glyph Bool)

prop_glyphApplicativeInterchange :: ArbGlyph (Fun Int8 Float) -> Int8 -> Bool
prop_glyphApplicativeInterchange (Gly u') y =
  let u = fmap applyFun u' in (u <*> pure y) == (pure ($ y) <*> u)


newtype ArbGlyph e = Gly (Glyph e)
  deriving newtype (Eq, Show)

instance Arbitrary e => Arbitrary (ArbGlyph e) where
  arbitrary = arbitrary1

instance Arbitrary1 ArbGlyph where
  liftArbitrary gen = frequency
    [ (1, pure $ Gly PeriodGlyph)
    , (19, Gly <$> (ArrayGlyph <$> fmap Vertical (f gen)
                               <*> fmap Horizontal (f gen)))
    ]
    where f = fmap (listArray glyphBounds) . vectorOf 20
