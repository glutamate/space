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

type family Plus a b :: *

type instance Plus Z a = a
type instance Plus (S b) a = S (Plus b a)

--type family NDims s :: *

--class NDims s where
--   type ND s :: *

--data RealWorld
--type instance NDims RealWorld = Three

--instance NDims RealWorld where
--    type ND RealWorld = Three

