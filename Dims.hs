{-# LANGUAGE EmptyDataDecls, UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}

module Dims where

data TTrue
data TFalse

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

type family Plus a b :: *

type instance Plus a Z = a
type instance Plus a (S b) = Plus (S a) b

type family NDims s :: *

--class NDims s where
--   type ND s :: *

data RealWorld
type instance NDims RealWorld = Three

--instance NDims RealWorld where
--    type ND RealWorld = Three

vecDims :: Vec n a -> Int
vecDims (VNil) = 0
vecDims (_:::vs) = 1 + vecDims vs

infixr 2 :::
infixr 2 .::

x .:: y = x ::: y :::VNil


data Vec n a where
   VNil :: Vec Z a
   (:::) :: a -> Vec m a -> Vec (S m) a

instance Functor (Vec n) where
    fmap f VNil = VNil
    fmap f (x:::vx) = (f x) ::: fmap f vx

vop2 :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vop2 f VNil VNil = VNil
vop2 f (x:::vx) (y:::vy) =  (f x y) ::: vop2 f vx vy
vop2 f _ _ = undefined

instance Eq a => Eq (Vec n a) where
   VNil == VNil = True
   (x ::: vx) == (y ::: vy) = x==y && vx == vy
   _ == _ = False

instance Show a => Show (Vec n a) where
   show VNil = "VNil"
   show (x :::vx) = show x ++":::"++show vx

instance Num a => Num (Vec n a) where
   vx + vy = vop2 (+) vx vy
   vx - vy = vop2 (-) vx vy
   vx * vy = vop2 (*) vx vy
   abs = fmap abs
   signum = fmap signum
   fromInteger i = error $ "fromInteger on vector" 

{-instance (Num a, Num (Vec n a)) => Num (Vec (S n) a) where
   vx + vy = vop2 (+) vx vy
   vx - vy = vop2 (-) vx vy
   vx * vy = vop2 (*) vx vy
   abs = fmap abs
   signum = fmap signum
   fromInteger i = VCons (fromInteger i) $ fromInteger i -}

uvx :: Vec Three Double
uvx = 1 ::: 0 .:: 0

uvy :: Vec Three Double
uvy = 0 ::: 1 ::: 0 ::: VNil

uvz :: Vec Three Double
uvz = 0 ::: 0 ::: 1 ::: VNil

orig :: Vec Three Double
orig = 0::: 0 ::: 0 ::: VNil

(.*) :: Double -> Vec n Double -> Vec n Double
(.*) = scalarMul

scalarMul :: Double -> Vec n Double -> Vec n Double
scalarMul scalar v = fmap (*scalar) v

vecIx :: Int -> Vec n a -> a
vecIx 0 (x:::_) = x
vecIx n (x:::xs) = vecIx (n-1) xs
vecIx _ VNil = error "vecIx out of bounds"

safevecIx :: Int -> Vec n a -> Maybe a
safevecIx 0 (x:::_) = Just x
safevecIx n (x:::xs) = safevecIx (n-1) xs
safevecIx _ VNil = Nothing
