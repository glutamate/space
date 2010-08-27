{-# OPTIONS_GHC -fglasgow-exts #-}

module Dims where

--type-level integers
data Z 
data S a

--we will cleanly separate coordinate systems with phantom types, runST-style
--but every coordinate system must have a number of dimensions
class NDims s where
   type ND s :: *

--Vectors
data Vec n a where
   VNil :: Vec Z a
   VCons :: a -> Vec m a -> Vec (S m) a

instance Functor (Vec n) where
    fmap f VNil = VNil
    fmap f (VCons x vx) = VCons (f x) $ fmap f vx

vop2 :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vop2 f VNil VNil = VNil
vop2 f (VCons x vx) (VCons y vy) = VCons (f x y) $ vop2 f vx vy

instance Eq a => Eq (Vec n a) where
   VNil == VNil = True
   VCons x vx == VCons y vy = x==y && vx == vy

instance Show a => Show (Vec n a) where
   show VNil = "VNil"
   show (VCons x vx) = show x ++"::"++show vx

instance Num a => Num (Vec n a) where
   vx + vy = vop2 (+) vx vy
   vx - vy = vop2 (-) vx vy
   vx * vy = vop2 (*) vx vy
   abs = fmap abs
   signum = fmap signum
   fromInteger i = undefined --VNil

{-instance (Num a, Num (Vec n a)) => Num (Vec (S n) a) where
   vx + vy = vop2 (+) vx vy
   vx - vy = vop2 (-) vx vy
   vx * vy = vop2 (*) vx vy
   abs = fmap abs
   signum = fmap signum
   fromInteger i = VCons (fromInteger i) $ fromInteger i -}

--a point in space, with an associated value
data Point s a where
     Point:: NDims s => Vec (ND s) Double -> a -> Point s a

translatePoint :: NDims s => Vec (ND s) Double -> Point s a -> Point s a
translatePoint vectr (Point vecpt x) = Point (vecpt+vectr) x
