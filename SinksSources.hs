{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module SinksSources where

import System.Environment
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

newtype Source b = Source { unSource :: Q Exp } -- IO b
newtype Sink b = Sink { unSink :: Q Exp } -- a-> b -> IO ()

args :: Source [String]
args =  Source [|  getArgs |]

putStr_ :: Sink String
putStr_ = Sink [| putStr |]

instance Lift () where
    lift () = [| () |]

readSource :: String -> Source a -> Q [Dec]
readSource nm (Source f) = do
    (src::Exp) <- f
    let rhs = VarE (mkName "unsafePerformIO") `AppE` (src) 
    return [ValD (VarP (mkName nm)) (NormalB rhs) []]
    --[d| nm = unsafePerformIO (print 5) |]

writeSink :: Lift b => b -> Sink b -> Q [Dec]
writeSink x (Sink f) = do
    xe <- lift x
    (src::Exp) <- f
    let rhs = VarE (mkName "unsafePerformIO") `AppE` (src `AppE` xe) 
    nm <- newName "sink"
    return [ValD (VarP nm) (NormalB rhs) []]

infixl 1 <*
x <* y = readSource x y

