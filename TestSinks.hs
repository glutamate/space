{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Main where

import SinksSources
import Language.Haskell.TH
import System.IO.Unsafe
 
--readSource "foo" args 
foo <* args  

foz <* argz ()

--sig = {: 1 :}

--z <- readFile "fool"

bar *> putStr_  
baz *> putStr_ 4

{-x = let y = 5
    in y

main = do print "hello"  
          let baz = 4::Int
          exp <- runQ [d| x = unsafePerformIO (print baz) |]
          print exp 
          print foo -}