{-# OPTIONS_GHC -fglasgow-exts #-}
{- OPTIONS_GHC -fwarn-incomplete-patterns -}

module GeneralizedSignal where

--import 
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.ST.Strict as SVST
import Foreign.Storable
--import EvalM
import Foreign.Storable.Tuple
import Data.List
import Control.Monad.ST
import Control.Monad
import TNUtils
import Dims
import PlotGnuplot
import Data.Ix
import Data.Complex
import Numeric.FFT

newtype Time = Time Double deriving (Eq, Show, Num, Fractional, Floating, Discretizable)
newtype Freq = Freq Double deriving (Eq, Show, Num, Fractional, Floating, Discretizable)
newtype Length = Length Double deriving (Eq, Show, Num, Fractional, Floating, Discretizable)
newtype SpaceFreq = SpaceFreq Double deriving (Eq, Show, Num, Fractional, Floating, Discretizable)

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

type family Inv a :: *
type instance Inv Time = Freq
type instance Inv Freq = Time
type instance Inv Length = SpaceFreq
type instance Inv SpaceFreq = Length
type instance Inv (Vec n a) = Vec n (Inv a)

data Signal a b where
    Signal  :: (Discretizable a, {-Ix (Discretized a),-} Storable b) =>  
               a -> (a,a) -> (Inv a, Inv a) -> SV.Vector b -> Signal a b
    SigFmap :: (b->b') -> Signal a b -> Signal a b'
    SigFun  :: (a->b) -> Signal a b

instance Functor (Signal a) where
    fmap f (SigFmap g s) = SigFmap (f . g) s
    fmap f s = SigFmap f s


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

class X a b where
    transform :: Signal a b -> Signal (Inv a) b

instance X Time (Complex Double) where
    transform (Signal d lims invlims arr) = Signal undefined invlims lims $ SV.pack $ fft $ SV.unpack arr
    

instance (Storable (b,c), X a b, X a c) => X a (b,c) where
    transform s = zipSignalsWith (,) (transform $ fmap fst s) (transform $ fmap snd s)