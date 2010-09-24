{-# OPTIONS_GHC -fglasgow-exts #-}

module Volume where 

import Dims

--typeclass approach
class Volume v where
      inside :: v s a -> Point s b -> Bool
     
data FRep s a = FRep (Vec (ND s) Double -> Double) a

instance Volume FRep where
         inside (FRep f _) (Point v _) = f v > 0

data Cuboid s a = Cuboid (Vec (ND s) Double) a

--need vzip = vop2 (,), vall,vany, vec2list

instance Volume Cuboid where
         inside (Cuboid vd _) (Point vp _) 
             = let p (cd,cp) = cp >0 && cp <cd
               in vall p $ vzip vd vp

data Spheroid s a = Spheroid (Vec (ND s) Double) a

instance Volume Spheroid where
         inside (Spheroid vd _) (Point vp _) 
             = let p (cd,cp) = cp >0 && cp <cd
               in vall p $ vzip vd vp

data Translate v = T (Vec (ND s) Double) (v s a)

instance Volume v => Volume (Translate v) where