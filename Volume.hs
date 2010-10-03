{-# OPTIONS_GHC -fglasgow-exts #-}

module Volume where 
import Nats
import VectorsL
--import Space

--typeclass approach
class Volume v where
      inside :: v n a -> Vec n Double -> Bool
     
data FRep n a = FRep (Vec n Double -> Double) a

instance Volume FRep where
         inside (FRep f _) v = f v > 0

data Cuboid n a = Cuboid (Vec n Double) a

--need vzip = vop2 (,), vall,vany, vec2list

instance Volume Cuboid where 
         inside (Cuboid vd _) (vp) 
             = let p (cd,cp) = cp >0 && cp <cd
               in vall p $ vzip vd vp

data Spheroid n a = Spheroid (Vec n Double) a

instance Volume Spheroid where
         inside (Spheroid vd _) vp 
             = vsum (vop2 (/) (squarev vp) (squarev vd)) < 1.0
             
data Translated v n a where
   Translated :: Nat n => Vec n Double -> v n a -> Translated v n a

instance Volume v => Volume (Translated v) where
    inside (Translated vtr vol) vp = inside vol $ (vp-vtr)

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

