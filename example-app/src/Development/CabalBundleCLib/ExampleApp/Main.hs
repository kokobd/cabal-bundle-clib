module Development.CabalBundleCLib.ExampleApp.Main
  ( main
  ) where

main :: IO ()
main = do
  putStrLn "Hello from Haskell"
  cpp_hello
  putStrLn "Back to Haskell again"

foreign import ccall unsafe "cpp_hello"
  cpp_hello :: IO ()
