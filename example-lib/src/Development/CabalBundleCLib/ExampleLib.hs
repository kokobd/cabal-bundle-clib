module Development.CabalBundleCLib.ExampleLib
  ( cHello
  ) where

cHello :: IO ()
cHello = c_hello

foreign import ccall "cpp_hello"
  c_hello :: IO ()
