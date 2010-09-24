{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Space where

import Dims 
--import Data.Array
--import qualified Prelude
--import Data.Array.Parallel.Prelude 
--import Data.Array.Parallel.Prelude.Double hiding (+)

data Point s a where 
     Point:: Vec (NDims s) Double -> a -> Point s a

instance Functor (Point s) where 
    fmap f (Point v x) = Point v $ f x

data Polygon s a where
     Poly :: [Point s ()] -> a -> Polygon s a 

instance Functor (Polygon s) where 
    fmap f (Poly pts x) = Poly pts $ f x
 
unitBox :: (NDims s ~ Three) => [Polygon s ()]
unitBox = uB where
  fz = Poly [pnt orig, pnt uvx, pnt (uvx+uvy), pnt uvy] ()
  fx = Poly [pnt orig, pnt uvy, pnt (uvz+uvy), pnt uvz] ()
  fy = Poly [pnt orig, pnt uvz, pnt (uvz+uvx), pnt uvx] ()
  uB = [fz, fx, fy, translate uvz fz, translate uvx fx, translate uvy fy] 

polyTag :: a -> Polygon s () -> Polygon s a
polyTag x (Poly p _) =  Poly p x

stretchPoly :: Vec (NDims s) Double -> Polygon s a -> Polygon s a
stretchPoly v (Poly pts a) = Poly (map g pts) a
   where g (Point vp x) = Point (vp*v) x

data Shape s a where
    Polys :: [Polygon s a] -> Shape s a
    Translate :: Vec (NDims s) Double -> Shape s a -> Shape s a

instance Functor (Shape s) where 
    fmap f (Polys polys) = Polys $ map (fmap f) polys
    fmap f (Translate v shp) = Translate v $ fmap f shp

data s :-> t where
    Linear :: (NDims s ~ NDims t) =>
              Vec (NDims s) (Vec (NDims t) Double) ->  s :-> t
    NonLinear :: (NDims s ~ NDims t) =>
                 (Point s a -> Point t a) -> s :-> t

class Transform f where
    translate :: Vec (NDims s) Double -> f s a -> f s a
--    transform :: (s :-> t) -> f s a -> f t a

instance Transform Point where
    translate vectr (Point vecpt x) = Point (vecpt+vectr) x
--    transform 
instance Transform Polygon where
    translate vec (Poly pts x) = Poly (translates vec pts) x

instance Transform Shape where
    translate vec shp = Translate vec shp

translates :: (Transform f) => Vec (NDims s) Double -> [f s a] -> [f s a]
translates v = map $ translate v

instance Show a => Show (Point s a) where
    show (Point v x) = "P "++show v ++ " "++show x

instance Show a => Show (Polygon s a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x

pnt :: Vec (NDims s) Double -> Point s ()
pnt v = Point v ()

tag x =fmap (const x)
tags x = map $ tag x

class HasSpace r where
    type TheSpace r :: *
    toSpace :: TheSpace r ~ s => r -> s 
    toSpace = undefined 

instance HasSpace (Polygon s a) where
    type TheSpace (Polygon s a) = s

instance HasSpace t => HasSpace [t] where
    type TheSpace [t] = TheSpace t