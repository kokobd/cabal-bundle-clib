import           Development.CabalBundleCLib       (mainWithCLib)
import           Development.CabalBundleCLib.CMake (simpleCMakeBuilder)

main :: IO ()
main = mainWithCLib
  "cpp_proj"
  ["mycpplib"]
  ["."]
  simpleCMakeBuilder
