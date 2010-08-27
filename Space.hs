{-# OPTIONS_GHC -fglasgow-exts #-}

module Space where

import Dims 
import Data.Array.Storable
import Foreign.Storable


data Point s a where 
     Point:: NDims s => Vec (ND s) Double -> a -> Point s a

data Image s a where
     ImArray :: (Storable a, NDims s) => StorableArray (Int,Int) a -> Image s a
     ImFmap :: (a->b) -> Image s a -> Image s b

data Polygon s a where
     Poly :: [Point s ()] -> a -> Polygon s a 

translatePoly :: NDims s => Vec (ND s) Double -> Polygon s a -> Polygon s a
translatePoly vec (Poly pts x) = Poly (map (translatePoint vec) pts) x

translatePoint :: NDims s => Vec (ND s) Double -> Point s a -> Point s a
translatePoint vectr (Point vecpt x) = Point (vecpt+vectr) x


instance Show a => Show (Point s a) where
    show (Point v x) = "P "++show v ++ " "++show x

instance Show a => Show (Polygon s a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x


--data s :-> t where
--     Linear :: (NDims