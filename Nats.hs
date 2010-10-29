{-# LANGUAGE EmptyDataDecls, UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}

module Nats where

import Data.Ix
import Foreign.Storable
import Foreign.Ptr

data TTrue
data TFalse

data Z 

newtype S a = S { unS :: a }

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three
type Five = S Four

type family Plus a b :: *

type instance Plus Z a = a
type instance Plus (S b) a = S (Plus b a)

class Nat n where
      toInt :: n -> Int

instance Nat Z where
         toInt _ = 0

instance Nat a => Nat (S a) where
    toInt = (1+) . toInt . unS
