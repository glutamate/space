{-# OPTIONS_GHC -fglasgow-exts #-}

module Volume where 

import Dims
import Space

--typeclass approach
class Volume v where
      inside :: v s a -> Point s b -> Bool
     
data FRep s a = FRep (Vec (NDims s) Double -> Double) a

instance Volume FRep where
         inside (FRep f _) (Point v _) = f v > 0

data Cuboid s a = Cuboid (Vec (NDims s) Double) a

--need vzip = vop2 (,), vall,vany, vec2list

instance Volume Cuboid where 
         inside (Cuboid vd _) (Point vp _) 
             = let p (cd,cp) = cp >0 && cp <cd
               in vall p $ vzip vd vp

data Spheroid s a = Spheroid (Vec (NDims s) Double) a

instance Volume Spheroid where
         inside (Spheroid vd _) (Point vp _) 
             = vsum (vop2 (/) (squarev vp) (squarev vd)) < 1.0
             
data Translated v s a where
   Translated ::  Vec (NDims s) Double -> v s a -> Translated v s a

instance Volume v => Volume (Translated v) where
    inside (Translated vtr vol) (Point vp tg) = inside vol $ Point (vp-vtr) tg

data Union v1 v2 s a where
   Union :: v1 s a -> v2 s a -> Union v1 v2 s a

instance (Volume v1, Volume v2) => Volume (Union v1 v2) where
    inside (Union v1 v2) pt = inside v1 pt || inside v2 pt

data Intersection v1 v2 s a where
   Intersection :: v1 s a -> v2 s a -> Intersection v1 v2 s a

instance (Volume v1, Volume v2) => Volume (Intersection v1 v2) where
    inside (Intersection v1 v2) pt = inside v1 pt && inside v2 pt

data VMinus v1 v2 s a where
   VMinus :: v1 s a -> v2 s a -> VMinus v1 v2 s a

instance (Volume v1, Volume v2) => Volume (VMinus v1 v2) where
    inside (VMinus v1 v2) pt = inside v1 pt && not (inside v2 pt)

--GADT approach

data VolumeG v s a where
    Vol :: v -> a -> (v -> Vec (NDims s) Double -> Bool) -> VolumeG v s a

insideG :: VolumeG v s a -> Point s b -> Bool
insideG (Vol vv _ f) (Point v _) = f vv v

