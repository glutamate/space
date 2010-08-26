{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Dims where

data Z 

newtype S a = S { unS :: a }

type One = S Z
type Two = S One
type Three = S Two

class TyInt a where
    toInt :: a -> Int

instance TyInt Z where
    toInt _ = 0

instance TyInt a => TyInt (S a) where
    toInt = (1+) . toInt . unS

class NDims a n | a -> n

data RealWorld

instance NDims RealWorld Three

data Vec n a where
   VNil :: Vec Z a
   VCons :: a -> Vec m a -> Vec (S m) a