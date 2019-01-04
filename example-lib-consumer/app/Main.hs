module Main where

import           Development.CabalBundleCLib.ExampleLib (cHello)

main :: IO ()
main = do
  putStrLn "Hello from Haskell."
  cHello
  putStrLn "Back to Haskell code."
