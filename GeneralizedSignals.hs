{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}
{- OPTIONS_GHC -fwarn-incomplete-patterns -}

module GeneralizedSignals where

--import 
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector.ST.Strict as SVST
import Foreign.Storable
--import EvalM
import Foreign.Storable.Tuple
import Data.List
import Control.Monad.ST
import Control.Monad
import TNUtils
import VectorsL
import Nats
--import PlotGnuplot 
import Data.Ix 
import Data.Complex
import Numeric.FFT 
import Math.FFT
import qualified Data.Array.CArray as CA
import System.IO.Unsafe
 
newtype Time = Time Double 
    deriving (Eq, Show, Num, Fractional, Floating, Discretizable, IsDouble)
newtype Freq = Freq Double 
    deriving (Eq, Show, Num, Fractional, Floating, Discretizable, IsDouble)
newtype Length = Length Double 
    deriving (Eq, Show, Num, Fractional, Floating, Discretizable, IsDouble)
newtype SpaceFreq = SpaceFreq Double 
    deriving (Eq, Show, Num, Fractional, Floating, Discretizable, IsDouble)

class (Eq a,Show a,Num a,Fractional a,Floating a)=> IsDouble a where
    toDouble :: a -> Double
    fromDouble :: Double -> a

instance IsDouble Double where
    toDouble = id
    fromDouble = id

class Discretizable a where
    discreteIndex ::  (a,a) -> a -> a -> Int
    discreteRange :: (a,a) -> a -> [a]
    discreteRngLength :: (a,a) -> a -> Int
    foldRangeM :: Monad m => (a,a) -> a -> b -> (a -> b -> m b) -> m b

instance Discretizable Double where
    discreteIndex (lo,hi) dx x = round $ (x-lo)/dx
    discreteRange (lo,hi) dx = [lo, lo+dx..hi]
    discreteRngLength (lo,hi) dx = round $ (hi-lo)/dx
    foldRangeM (lo,hi) dx init f = 
        let nmax = discreteRngLength (lo,hi) dx
            go n acc | n > nmax = return acc
                     | otherwise = do
              next <- f ((realToFrac n)*dx+lo) acc
              go (n+1) next
        in go 0 init

instance Discretizable Int where
    discreteIndex (lo,hi) 1 x = x-lo
    discreteIndex (lo,hi) dx x = (x-lo) `div` dx
    discreteRange (lo,hi) dx = [lo,lo+dx..hi]
    discreteRngLength (lo,hi) dx = (hi-lo) `div` dx
    foldRangeM (lo,hi) dx init f = 
        let go n acc | n > hi = return acc
                     | otherwise = do
              next <- f n acc
              go (n+dx) next
        in go lo init


{-instance Discretizable a => Discretizable (Vec Z a) where
    discreteIndex (lo,hi) dx x = 0
    discreteRange (lo,hi) dx = [vnil]
    discreteRngLength (lo,hi) dx = 1
    foldRangeM (lo,hi) dx init f = return init -}

instance Discretizable a => Discretizable (Vec (S Z) a) where
    discreteIndex (lo,hi) dx x = discreteIndex (vcar lo,vcar hi) (vcar dx) (vcar x)
    discreteRange (lo,hi) dx = map (`vcons` vnil) $ discreteRange (vcar lo,vcar hi) (vcar dx)
    discreteRngLength (lo,hi) dx = discreteRngLength (vcar lo,vcar hi) (vcar dx)
    foldRangeM (lo,hi) dx init f = foldRangeM (vcar lo,vcar hi) (vcar dx) init f'
        where f' x acc = f (x `vcons` vnil) acc


instance (Discretizable a, Discretizable (Vec (S n) a)) => Discretizable (Vec (S (S n)) a) where
    discreteIndex (vx, vy) (vd) (vw) 
        = discreteIndex (vcar vx,vcar vy) (vcar vd) (vcar vw) + 
         (discreteIndex (vcar vx,vcar vy) (vcar vd) (vcar vy) + 1) * 
         discreteIndex (vcdr vx,vcdr vy) (vcdr vd) (vcdr vw)
    discreteRange (vx, vy) vd 
        = [ vcons xys vxsys | vxsys <- discreteRange (vcdr vx, vcdr vy) (vcdr vd),
                              xys <- discreteRange (vcar vx,vcar vy) (vcar vd)]
    discreteRngLength (vx, vy) (vd) = discreteRngLength (vcar vx,vcar vy) (vcar vd) * 
                                      discreteRngLength (vcdr vx,vcdr vy) (vcdr vd)
--    foldRangeM :: Monad m => (a,a) -> a -> b -> (a -> b -> m b) -> m b
    foldRangeM (vlo, vhi) vd  init f = 
      let f' x acc = 
             foldRangeM (vcdr vlo, vcdr vhi) (vcdr vd) acc $ \ix acc' -> f (x `vcons` ix) acc' 
      in foldRangeM (vcar vlo, vcar vhi) (vcar vd) init f'
      
f v (a, i) = return $ (v:a, i+1)

tf = print =<< foldRangeM (0::Vec Three Int, 2) (1) ([],0) f

tf2 = discreteIndex (0::Vec Three Int, 2) (1) (2 `vcons` 2 `vcons` 2 `vcons` vnil)


{-$ \x-> do        
         let thisf ix acc = f (x `vcons` ix) acc 
         foldRangeM (vcdr vlo, vcdr vhi) (vcdr vd) init thisf -}


type family Volume a :: *
type instance Volume Time = (Time,Time)
type instance Volume Freq = (Freq,Freq)

--type family FInverses a b :: * -> *

--type instance FInverses Time Freq

--type family FInv a :: *

--type instance FInv Time = Freq
--type instance (FInv t ~ s) => FInv s = t 

{-class Inverses a b | a -> b, b-> a where
    invertFwd :: a -> b
    invertBack :: b -> a

instance Inverses Time Freq where
    invertFwd (Time t) = Freq $ recip t
    invertBack (Freq f) = Time $ recip f

instance Inverses Length SpaceFreq where
    invertFwd (Length t) = SpaceFreq $ recip t
    invertBack (SpaceFreq f) = Length $ recip f-}

class HasInverse a where
      type Inv a :: *
      invertValue :: a -> Inv a

instance HasInverse Int where
      type Inv Int = Int
      invertValue = negate -- haha. FIXME

instance HasInverse Time where
      type Inv Time = Freq
      invertValue (Time t) = Freq $ recip t

instance HasInverse Freq where
      type Inv Freq = Time
      invertValue (Freq t) = Time $ recip t

instance HasInverse Length where
      type Inv Length = SpaceFreq
      invertValue (Length t) = SpaceFreq $ recip t

instance HasInverse SpaceFreq where
      type Inv SpaceFreq = Length
      invertValue (SpaceFreq t) = Length $ recip t

instance HasInverse a => HasInverse (Vec n a) where
      type Inv (Vec n a) = Vec n (Inv a)
      invertValue = fmap invertValue

data Signal a b where
    Signal  :: (Discretizable a, {-Ix (Discretized a),-} Storable b) =>  
               a -> (a,a) -> (Inv a, Inv a) -> SV.Vector b -> Signal a b
    SigFmap :: (b->b') -> Signal a b -> Signal a b'
    SigFun  :: (a->b) -> Signal a b

sigLims :: Signal a b -> (a,a)
sigLims (Signal d1 lims1 invlims1 arr1) = lims1
sigLims (SigFmap _ s) = sigLims s

instance Functor (Signal a) where
    fmap f (SigFmap g s) = SigFmap (f . g) s
    fmap f s = SigFmap f s

sOfVToVofS :: Signal a (Vec n b) -> Vec n (Signal a b)
sOfVToVofS s = undefined

vOfSToSofV :: Vec n (Signal a b) -> Signal a (Vec n b) 
vOfSToSofV voss = let sigs = vecToList voss
                  in undefined


type Events a b = [(a,b)]
type Region a b = [(Volume a,b)]

at :: Signal a b -> a -> b
at (Signal delta lims _ arr) x =  arr `SV.index` (discreteIndex lims delta x)
at (SigFmap f s) x = f $ s `at` x
at (SigFun f) x = f x

fill :: (Discretizable a, Storable b) => 
        a -> (a,a) -> (Inv a, Inv a) -> (a->b) -> Signal a b
fill delta lims invlims f = 
     Signal delta lims invlims $ SV.pack $ map f $ discreteRange lims delta

fillM :: (Discretizable a, Storable b, Monad m) => 
          a -> (a,a) -> (Inv a, Inv a) -> (a->m b) -> m (Signal a b)
fillM delta lims invlims mf = do
  let rng = discreteRange lims delta
  let n = discreteRngLength lims delta
  xs <- mapM mf $ rng
  return $ Signal delta lims invlims $ SV.pack xs 


zipSignalsWith :: Storable c => (a -> b -> c) -> Signal i a -> Signal i b -> Signal i c
zipSignalsWith f (Signal d1 lims1 invlims1 arr1) (Signal d2 lims2 invlims2 arr2)
               = Signal d1 lims1 invlims2 $ SV.zipWith f arr1 arr2

--toCArray :: Signal a b -> CArray 

class X a b where
    fourier :: Signal a b -> Signal (Inv a) b

instance X Time (Complex Double) where
    fourier (Signal d lims invlims arr) = Signal (invertValue d) invlims lims $ SV.pack $ fft $ SV.unpack arr

instance X Freq (Complex Double) where
    fourier (Signal d lims invlims arr) = Signal (invertValue d) invlims lims $ SV.pack $ ifft $ SV.unpack arr

instance X Length (Complex Double) where
    fourier (Signal d lims invlims arr) = Signal (invertValue d) invlims lims $ SV.pack $ fft $ SV.unpack arr

instance X SpaceFreq (Complex Double) where
    fourier (Signal d lims invlims arr) = Signal (invertValue d) invlims lims $ SV.pack $ ifft $ SV.unpack arr

--instance X (Vec Z Length) (Complex Double) where
--    fourier = undefined
 
instance (Nat n, X (Vec n Length) (Complex Double), 
          Discretizable (Vec n SpaceFreq), Ix (Vec n Int), CA.Shapable (Vec n Int)) 
    => X (Vec n Length) (Complex Double) where
    fourier s@(Signal d lims invlims arr) = 
        let ixdims :: Nat n => Signal (Vec n a) b -> n
            ixdims = undefined
            dims = toInt $ ixdims s
            (oldp, oldn, oldsomething) = SVB.toForeignPtr arr
            cixs = discreteLims s
            carray = unsafePerformIO (CA.unsafeForeignPtrToCArray oldp cixs)
            newcarray = dftN [0..dims] carray
            (newn, newp) = CA.toForeignPtr newcarray
            newarr = SVB.fromForeignPtr newp newn
        in Signal (invertValue d) invlims lims newarr
    
--instance IsDouble d => Shapable (Vec Z d) where

discreteLims :: (Nat n, IsDouble d) => Signal (Vec n d) a -> (Vec n Int, Vec n Int)
discreteLims s@(Signal d (lo,hi) invlims arr) 
    = ( fmap (round . toDouble) $ lo/d,  fmap (round . toDouble) $ hi/d)


instance (Storable (b,c), X a b, X a c) => X a (b,c) where
    fourier s = zipSignalsWith (,) (fourier $ fmap fst s) (fourier $ fmap snd s)

instance (X a b) => X a (Vec n b) where
    fourier sOfVecs =  vOfSToSofV $ fmap fourier $ sOfVToVofS sOfVecs

class XRC a where
    fourierRC :: Signal a Double -> Signal (Inv a) (Complex Double)
    fourierCR :: Signal (Inv a) (Complex Double) -> Signal a Double