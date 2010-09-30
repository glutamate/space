{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}
{- OPTIONS_GHC -fwarn-incomplete-patterns -}

module GeneralizedSignal where

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
import Dims
--import PlotGnuplot 
import Data.Ix 
import Data.Complex
import Numeric.FFT
import Math.FFT
import Data.Array.CArray
import System.IO.Unsafe
import Data.Ix.Shapable

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

instance Discretizable Double where
    discreteIndex (lo,hi) dx x = round $ (x-lo)/dx
    discreteRange (lo,hi) dx = [lo, lo+dx..hi]

instance Discretizable Int where
    discreteIndex (lo,hi) 1 x = x-lo
    discreteIndex (lo,hi) dx x = (x-lo) `div` dx
    discreteRange (lo,hi) dx = [lo,lo+dx..hi]

instance Discretizable a => Discretizable (Vec Z a) where
    discreteIndex (lo,hi) dx x = 0
    discreteRange (lo,hi) dx = [VNil]

instance (Discretizable a, Discretizable (Vec n a)) => Discretizable (Vec (S n) a) where
    discreteIndex (x ::: xs, y ::: ys) (dx:::dxs) (w:::ws) 
        = discreteIndex (x,y) dx w + (discreteIndex (x,y) dx y + 1) * discreteIndex (xs,ys) dxs ws
    discreteRange (x ::: xs, y ::: ys) (dx:::dxs) 
        = [ xys:::xsys | xsys <- discreteRange (xs, ys) dxs, 
                         xys <- discreteRange (x,y) dx]

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

instance Functor (Signal a) where
    fmap f (SigFmap g s) = SigFmap (f . g) s
    fmap f s = SigFmap f s

sOfVToVofS :: Signal a (Vec n b) -> Vec n (Signal a b)
sOfVToVofS s = undefined

vOfSToSofV :: Vec n (Signal a b) -> Signal a (Vec n b) 
vOfSToSofV s = undefined


type Events a b = [(a,b)]
type Region a b = [(Volume a,b)]

at :: Signal a b -> a -> b
at (Signal delta lims _ arr) x =  arr `SV.index` (discreteIndex lims delta x)
at (SigFmap f s) x = f $ s `at` x
at (SigFun f) x = f x

fill :: (Discretizable a, Storable b) => a -> (a,a) -> (Inv a, Inv a) -> (a->b) -> Signal a b
fill delta lims invlims f = Signal delta lims invlims $ SV.pack $ map f $ discreteRange lims delta

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
 
instance (TyInt n, X (Vec n Length) (Complex Double), 
          Discretizable (Vec n SpaceFreq), Ix (Vec n Int), Shapable (Vec n Int)) 
    => X (Vec n Length) (Complex Double) where
    fourier s@(Signal d lims invlims arr) = 
        let ixdims :: TyInt n => Signal (Vec n a) b -> n
            ixdims = undefined
            dims = toInt $ ixdims s
            (oldp, oldn, oldsomething) = SVB.toForeignPtr arr
            cixs = discreteLims s
            carray = unsafePerformIO (unsafeForeignPtrToCArray oldp cixs)
            newcarray = dftN [0..dims] carray
            (newn, newp) = toForeignPtr newcarray
            newarr = SVB.fromForeignPtr newp newn
        in Signal (invertValue d) invlims lims newarr
    
--instance IsDouble d => Shapable (Vec Z d) where

discreteLims :: IsDouble d => Signal (Vec n d) a -> (Vec n Int, Vec n Int)
discreteLims s@(Signal d (lo,hi) invlims arr) = ( roundV $ fmap toDouble $ lo/d,  roundV $fmap toDouble$ hi/d)

instance Shapable (Vec Z Int) where
    sRank VNil = 0
    sShape VNil VNil = []
    sBounds [] = (VNil, VNil)

instance Shapable (Vec n Int) => Shapable (Vec (S n) Int) where
    sRank (x:::xs) = 1+sRank xs
    sShape (x:::xs) (y:::ys) = rangeSize (x,y):sShape xs ys
    sBounds (x:xs) = let (lo,hi) = sBounds xs
                     in (0:::lo, (x-1):::hi)


instance (Storable (b,c), X a b, X a c) => X a (b,c) where
    fourier s = zipSignalsWith (,) (fourier $ fmap fst s) (fourier $ fmap snd s)

instance (X a b) => X a (Vec n b) where
    fourier sOfVecs =  vOfSToSofV $ fmap fourier $ sOfVToVofS sOfVecs

class XRC a where
    fourierRC :: Signal a Double -> Signal (Inv a) (Complex Double)
    fourierCR :: Signal (Inv a) (Complex Double) -> Signal a Double