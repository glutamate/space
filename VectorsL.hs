{-# LANGUAGE EmptyDataDecls, UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}

module VectorsL where

import Data.Ix
import Foreign.Storable
import Foreign.Ptr
import Nats
import Data.Ix.Shapable


newtype Vec n a = V [a]

instance Functor (Vec n) where
    fmap f (V xs) = V $ map f xs

instance Ord a => Ord (Vec n a) where
    V xs <= V ys = xs <= ys

instance Ix a => Ix (Vec Z a) where
    range (_, _) = [V []]
    inRange _ _ = True
    index _ _ = 0

vcdr :: Vec (S n) a -> Vec n a
vcdr (V xs) = V $ tail xs

vcar :: Vec (S n) a -> a
vcar (V xs) = head xs


infixr 2 `vcons`

vcons :: a -> Vec n a -> Vec (S n) a
vcons x (V xs) = V (x:xs)

vnil :: Vec Z a
vnil = V []

vecIx :: Int -> Vec n a -> a
vecIx n (V xs) = xs !!n

v ! i = vecIx i v

vecToList :: Vec n a -> [a]
vecToList (V xs) = xs

vop2 :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vop2 f (V vx) (V vy) =  V $ zipWith f vx vy

vfold :: (a -> b -> a) -> a -> Vec n b -> a
vfold f acc (V xs) =  foldl f acc xs

vapp :: Vec n a -> Vec m a -> Vec (Plus n m) a
vapp (V xs) (V ys) = V $ xs ++ ys

vrev :: Vec n a -> Vec n a
vrev (V xs) = V $ reverse xs

vsnoc ::  Nat n => Vec n a -> a -> Vec (S n) a
vsnoc (V v) x = V $ v++[x]


mkVec :: Nat n => a -> Vec n a
mkVec x = v
      where n = vdims v
            v = V $ replicate n x
      
incVec :: Nat n => Vec n Int
incVec = v
      where n = vdims v
            v = V $ take n [1..]


instance (Ix a,Ix (Vec  n a))  => Ix (Vec (S n) a) where
    range (vx, vy) = 
           [ vcons xys vxsys | vxsys <- range (vcdr vx, vcdr vy), 
                              xys <- range (vcar vx,vcar vy)] -- OK
    inRange (vx, vy) vw = inRange (vcar vx,vcar vy) (vcar vw) && 
                          inRange (vcdr vx,vcdr vy) (vcdr vw) -- OK

    index (vx, vy) vw= index (vcar vx,vcar vy) (vcar vw) + 
                       (index (vcar vx,vcar vy) (vcar vy) + 1) * 
                       index (vcdr vx,vcdr vy) (vcdr vw) -- OK

instance Storable a => Storable (Vec Z a) where
    sizeOf _ = 0
    alignment = alignment  . velem
    peek ptr = return $ vnil
    poke ptr v = return ()

instance (Nat n, Storable a, Storable (Vec n a)) => Storable (Vec (S n) a) where
    sizeOf v = vdims v*sizeOf (velem v) 
    alignment = alignment  . velem
    peek p = do 
      let q = castPtr p
      x <- peek q
      xs <- peek (plusPtr p $ sizeOf x)
      return $ vcons x xs
    poke p v = do
      poke (castPtr p) $ vcar v
      poke (plusPtr p $ sizeOf $ velem v) $ vcdr v


vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip = vop2 (,)

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

--vdims1 :: Vec n a -> Int
--vdims1 (V vs) = length vs

velem :: Vec n a -> a 
velem = undefined


t :: Vec Three Int
t = mkVec 1

t1 :: Vec Three Int
t1 = incVec

t2 = vapp t1 t

instance Eq a => Eq (Vec n a) where
   vx == vy = vecToList vx == vecToList vy

instance Show a => Show (Vec n a) where
   show vx = "V "++(show $ vecToList vx)

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
uvx = 1 `vcons` 0 `vcons` 0 `vcons` vnil

uvy :: Vec Three Double
uvy = 0 `vcons` 1 `vcons` 0 `vcons` vnil

uvz :: Vec Three Double
uvz = 0 `vcons` 0 `vcons` 1 `vcons` vnil

orig :: Vec Three Double
orig = 0 `vcons` 0 `vcons` 0 `vcons` vnil

(.*) :: Double -> Vec n Double -> Vec n Double
(.*) = scalarMul

scalarMul :: Double -> Vec n Double -> Vec n Double
scalarMul scalar v = fmap (*scalar) v


safevecIx :: Nat n => Int -> Vec n a -> Maybe a
safevecIx n v | n < vdims v = Just $ vecIx n v
              | otherwise = Nothing
 

instance Shapable (Vec Z Int) where
    sRank _ = 0
    sShape _ _ = []
    sBounds [] = (vnil, vnil)
    sBounds _ = undefined

instance (Nat n, Shapable (Vec n Int)) => Shapable (Vec (S n) Int) where
    sRank v = vdims v
    sShape vx vy = rangeSize (vcar vx,vcar vy):sShape (vcdr vx) (vcdr vy)
    sBounds (x:xs) = let (vlo, vhi) = sBounds xs
                     in (vcons 0 vlo, vcons (x-1) vhi)
    sBounds _ = undefined                               

