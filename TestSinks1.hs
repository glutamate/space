module Main where

import Language.Haskell.Exts

modDecls (Module _ _ _ _ _ _ decls) = decls


main = do
  mmod <- fromParseResult `fmap` parseFile "TestSinks.hs"
  mapM print $ modDecls mmod
  putStrLn "foo!"
 