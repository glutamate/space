{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Space where

import Dims 

data Point s a where 
     Point:: Vec (NDims s) Double -> a -> Point s a

instance Functor (Point s) where 
    fmap f (Point v x) = Point v $ f x

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

translates :: (Transform f) => Vec (NDims s) Double -> [f s a] -> [f s a]
translates v = map $ translate v 

instance Show a => Show (Point s a) where
    show (Point v x) = "P "++show v ++ " "++show x

pnt :: Vec (NDims s) Double -> Point s ()
pnt v = Point v ()

tag x =fmap (const x)
tags x = map $ tag x

class HasSpace r where
    type TheSpace r :: *
    toSpace :: TheSpace r ~ s => r -> s 
    toSpace = undefined 

instance HasSpace t => HasSpace [t] where
    type TheSpace [t] = TheSpace t

{-class HasSpace r s where
    toSpace1 :: r s a -> s 
    toSpace1 = undefined 

instance HasSpace t => HasSpace [t] where
    type TheSpace [t] = TheSpace t -}