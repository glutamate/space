{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Polygon where

import Nats
import VectorsL
import Space
import TNUtils

data Polygon n a where 
     Poly :: [Vec n Double] -> a -> Polygon n a 

data Quads n a where 
     Quads :: [(Vec Four (Vec n Double), a)] -> Quads n a 

quadMapVs :: (Vec Four (Vec n Double) -> Vec Four (Vec n Double))
     -> Quads n a
     -> Quads n a
quadMapVs f (Quads vs') = Quads $ map (\(vs,p)-> (f vs,p)) vs'

quadRev = quadMapVs vrev

instance Functor (Polygon n) where 
    fmap f (Poly pts x) = Poly pts $ f x
 
instance Functor (Quads n) where 
    fmap f (Quads pts) = Quads $ map (\(vs,p)-> (vs,f p)) pts

unitBox3 :: Quads Three ()
unitBox3 = uB where
  fz,fx, fy :: (Vec Four (Vec Three Double), ())
  fz =  ((orig`vcons`uvx`vcons`(uvx+uvy)`vcons`uvy`vcons`vnil), ())
  fx =  ((orig`vcons`uvy`vcons`(uvz+uvy)`vcons`uvz`vcons`vnil), ())
  fy =  ((orig`vcons`uvz`vcons`(uvz+uvx)`vcons`uvx`vcons`vnil), ())
  shiftBox = Quads [fz, fy,  fx , (onFst (fmap (+uvx) . vrev) fx),
                                      (onFst (fmap (+uvy) . vrev) fy),
                                      (onFst (fmap (+uvz) . vrev) fz) ] 
  uB = translate (negate $ uvx/2+uvz/2+uvy/2) $ shiftBox




stretchPoly :: Nat n => Vec n Double -> Polygon n a -> Polygon n a
stretchPoly v (Poly pts a) = Poly (map g pts) a
   where g (vp) = vp*v

{-data Shape n a where
    Polys :: [Polygon n a] -> Shape n a
    Translate :: Vec n Double -> Shape n a -> Shape n a 

instance Functor (Shape s) where 
    fmap f (Polys polys) = Polys $ map (fmap f) polys
    fmap f (Translate v shp) = Translate v $ fmap f shp -}

 
instance Transform Polygon where
    translate vec (Poly pts x) = Poly (map (+vec) pts) x
    stretch = stretchPoly

instance Transform Quads where
    translate vec = quadMapVs $ fmap (+vec)
    stretch v  = quadMapVs $ fmap g
          where g (vp) = vp*v

{-instance Transform Shape where
    translate vec shp = Translate vec shp -}

instance Show a => Show (Polygon n a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x



