{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Space where

import Dims 
--import Data.Array
--import qualified Prelude
--import Data.Array.Parallel.Prelude 
--import Data.Array.Parallel.Prelude.Double hiding (+)

data Point s a where 
     Point:: NDims s => Vec (ND s) Double -> a -> Point s a

instance Functor (Point s) where 
    fmap f (Point v x) = Point v $ f x

data Polygon s a where
     Poly :: [Point s ()] -> a -> Polygon s a 

instance Functor (Polygon s) where 
    fmap f (Poly pts x) = Poly pts $ f x

data Shape s a where
    Polys :: [Polygon s a] -> Shape s a
    Translate :: NDims s => Vec (ND s) Double -> Shape s a -> Shape s a

instance Functor (Shape s) where 
    fmap f (Polys polys) = Polys $ map (fmap f) polys
    fmap f (Translate v shp) = Translate v $ fmap f shp

data s :-> t where
    Linear :: (NDims s, NDims t, ND s ~ ND t) =>
              Vec (ND s) (Vec (ND t) Double) ->  s :-> t
    NonLinear :: (NDims s, NDims t, ND s ~ ND t) =>
                 (Point s a -> Point t a) -> s :-> t

class Transform f where
    translate :: NDims s => Vec (ND s) Double -> f s a -> f s a
--    transform :: (s :-> t) -> f s a -> f t a

instance Transform Point where
    translate vectr (Point vecpt x) = Point (vecpt+vectr) x
--    transform 
instance Transform Polygon where
    translate vec (Poly pts x) = Poly (translates vec pts) x

instance Transform Shape where
    translate vec shp = Translate vec shp

translates :: (NDims s, Transform f) => Vec (ND s) Double -> [f s a] -> [f s a]
translates v = map $ translate v

instance Show a => Show (Point s a) where
    show (Point v x) = "P "++show v ++ " "++show x

instance Show a => Show (Polygon s a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x

pnt :: NDims s => Vec (ND s) Double -> Point s ()
pnt v = Point v ()

tag x =fmap (const x)
tags x = map $ tag x
