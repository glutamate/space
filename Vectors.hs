{-# LANGUAGE EmptyDataDecls, UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}

module Vectors where

import Data.Ix
import Foreign.Storable
import Foreign.Ptr
import Nats
import Data.Ix.Shapable


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

instance Ord a => Ord (Vec n a) where
    (x ::: xs) <= (y ::: ys) = x <= y || xs <= ys
    VNil <= VNil = True 
    _ <= _ = undefined

instance Ix a => Ix (Vec Z a) where
    range (VNil, VNil) = [VNil]
    range _ = []
    inRange _ _ = True
    index _ _ = 0

instance (Ix a,Ix (Vec  n a))  => Ix (Vec (S n) a) where
    range (x ::: xs, y ::: ys) = [ xys:::xsys | xsys <- range (xs, ys), xys <- range (x,y)] -- OK
    range _ = []
    inRange (x ::: xs, y ::: ys) (w:::ws) = inRange (x,y) w && inRange (xs,ys) ws -- OK
    inRange _ _ = undefined
    index (x ::: xs, y ::: ys) (w:::ws) = index (x,y) w + (index (x,y) y + 1) * index (xs,ys) ws -- OK
    index _ _ = undefined

instance Storable a => Storable (Vec Z a) where
    sizeOf _ = 0
    alignment = alignment  . velem
    peek ptr = return VNil
    poke ptr v = return ()

instance (Storable a, Storable (Vec n a)) => Storable (Vec (S n) a) where
    sizeOf (x:::xs) = sizeOf x + sizeOf xs
    sizeOf _ = undefined
    alignment = alignment  . velem
    peek p = do 
      let q = castPtr p
      x <- peek q
      xs <- peek (plusPtr p $ sizeOf x)
      return (x ::: xs)
    poke p (x:::xs) = do
      poke (castPtr p) x
      poke (plusPtr p $ sizeOf x) xs
    poke p _ = undefined

vop2 :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vop2 f VNil VNil = VNil
vop2 f (x:::vx) (y:::vy) =  (f x y) ::: vop2 f vx vy
vop2 f _ _ = undefined

vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip = vop2 (,)

vfold :: (a -> b -> a) -> a -> Vec n b -> a
vfold f acc VNil = acc
vfold f acc (x:::xs) = vfold f (f acc x) xs

vany, vall :: (a->Bool) -> Vec n a -> Bool
vany p = vfold (\acc x -> p x || acc) False 
vall p = vfold (\acc x -> p x && acc) True 

vsum :: (Num a) => Vec n a -> a
vsum = vfold (+) 0 

squarev :: (Num a) => Vec n a -> Vec n a
squarev = fmap (\x->x*x)

vdims :: Nat n => Vec n a -> Int
vdims v = toInt $ n v where
    n :: Vec n a -> n
    n = undefined

vdims1 :: Vec n a -> Int
vdims1 VNil = 0
vdims1 (_ ::: vs) = 1+ vdims1 vs

velem :: Vec n a -> a 
velem = undefined

--mkVec :: Nat n => a -> Vec n a
--mkVec x = fmap (const x) undefinedVector
       
--mkVec1 :: n -> a -> Vec n a



{-vapp :: Vec n a -> Vec m a -> Vec (Plus n m) a
vapp VNil v = v
vapp (x:::xs) v = x ::: vapp xs v 

vsnoc ::  Nat n => Vec n a -> a -> Vec (S n) a
vsnoc v x = v `vapp` (x ::: VNil )

vrev :: Vec n a -> Vec n a
vrev VNil = VNil
vrev (x ::: xs) = vrev xs `vapp` (x ::: VNil ) -}

class Nat n where
    toInt :: n -> Int
    mkVec:: a -> Vec n a
    incVec :: Vec n Int

instance Nat Z where
    toInt _ = 0
    mkVec _ = VNil
    incVec = VNil

instance Nat a => Nat (S a) where
    toInt = (1+) . toInt . unS
    mkVec x = x ::: mkVec x
    incVec = v
        where d = vdims v
              v = d ::: incVec

t :: Vec Three Int
t = mkVec 1

t1 :: Vec Three Int
t1 = incVec

--t2 = vapp t1 t1

instance Eq a => Eq (Vec n a) where
   VNil == VNil = True
   (x ::: vx) == (y ::: vy) = x==y && vx == vy
   _ == _ = False

instance Show a => Show (Vec n a) where
   show VNil = "VNil"
   show (x :::vx) = show x ++":::"++show vx

instance (Num a, Nat n) => Num (Vec n a) where
   vx + vy = vop2 (+) vx vy
   vx - vy = vop2 (-) vx vy
   vx * vy = vop2 (*) vx vy
   abs = fmap abs
   signum = fmap signum
   fromInteger i = mkVec $ fromInteger i

instance (Fractional a, Nat n) => Fractional (Vec n a) where
   fromRational = mkVec . fromRational
   vx / vy = vop2 (/) vx vy

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
 

instance Shapable (Vec Z Int) where
    sRank VNil = 0
    sShape VNil VNil = []
    sBounds [] = (VNil, VNil)

instance Shapable (Vec n Int) => Shapable (Vec (S n) Int) where
    sRank (x:::xs) = 1+sRank xs
    sShape (x:::xs) (y:::ys) = rangeSize (x,y):sShape xs ys
    sBounds (x:xs) = let (lo,hi) = sBounds xs
                     in (0:::lo, (x-1):::hi)

