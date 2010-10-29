{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Space where

import Nats
import VectorsL

data Point n a where 
     Point::  Nat n => Vec n Double -> a -> Point n a

instance Functor (Point s) where 
    fmap f (Point v x) = Point v $ f x

{-data s :-> t where
    Linear :: (NDims s ~ NDims t) =>
              Vec (NDims s) (Vec (NDims t) Double) ->  s :-> t
    NonLinear :: (NDims s ~ NDims t) =>
                 (Point s a -> Point t a) -> s :-> t -}

class Transform f where
    translate ::  Nat n => Vec n Double -> f n a -> f n a
    stretch :: Nat n => Vec n Double -> f n a -> f n a
--    transform :: (s :-> t) -> f s a -> f t a

instance Transform Point where
    translate vectr (Point vecpt x) = Point (vecpt+vectr) x
    stretch v p = p
--    transform 

translates :: (Nat n, Transform f) => Vec n Double -> [f n a] -> [f n a]
translates v = map $ translate v 

instance Show a => Show (Point s a) where
    show (Point v x) = "P "++show v ++ " "++show x

--pnt :: Nat n => Vec n Double -> Point s ()
pnt v = Point v ()

tag x =fmap (const x)
tags x = map $ tag x
