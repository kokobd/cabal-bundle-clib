module Development.CabalBundleCLib
  ( mainWithCLib
  , module Development.CabalBundleCLib.Types
  ) where

import           Data.Maybe                         (fromJust)
import           Data.Time.Clock                    (getCurrentTime)
import           Development.CabalBundleCLib.Types
import qualified Distribution.PackageDescription    as Cabal
import qualified Distribution.Simple                as Cabal
import qualified Distribution.Simple.Setup          as Cabal
import qualified Distribution.Types.BuildInfo       as Cabal
import qualified Distribution.Types.HookedBuildInfo as Cabal
import qualified Distribution.Types.LocalBuildInfo  as Cabal
import           System.Directory                   (removeFile)
import           System.IO.Temp                     (writeTempFile)

mainWithCLib :: FilePath -- ^c project root
             -> [String] -- ^bundled libraries
             -> [FilePath] -- ^parent dirs of libraries (relative to build root)
             -> (BuildAction -> BuildDirs -> IO ()) -- ^builder
             -- The three 'FilePath's are source dir, build dir and installation
             -- dir, respectively
             -> IO ()
mainWithCLib cProjRoot bundledLibs bundledLibDirs builder = do
  putStrLn "defaultMainWithCMake not implemented yet"
  Cabal.defaultMainWithHooks Cabal.simpleUserHooks
    { Cabal.preBuild = customPreBuild bundledLibs bundledLibDirs
    , Cabal.buildHook = customBuild cProjRoot builder
    }

customPreBuild :: [String]
               -> [String]
               -> Cabal.Args
               -> Cabal.BuildFlags
               -> IO Cabal.HookedBuildInfo
customPreBuild bundledLibs bundledLibDirs _ _ = do
  let buildInfo = Cabal.emptyBuildInfo
        { Cabal.extraLibs = bundledLibs
        , Cabal.extraLibDirs = bundledLibDirs
        }
  pure $ (Just buildInfo, [])

customBuild :: FilePath -- ^c project root
            -> (BuildAction -> BuildDirs -> IO ())
            -> Cabal.PackageDescription
            -> Cabal.LocalBuildInfo
            -> Cabal.UserHooks
            -> Cabal.BuildFlags
            -> IO ()
customBuild cProjRoot builder packageDesc localBuildInfo userHooks buildFlags = do
  currentTime <- getCurrentTime
  let versionInfo = "const char *clibver = \"" ++ show currentTime ++ "\";\n"
  let buildDir = Cabal.buildDir localBuildInfo
  clibVersionFile <- writeTempFile buildDir "clibver.c" versionInfo
  let updateLibrary :: Cabal.Library -> Cabal.Library
      updateLibrary lib = lib
        { Cabal.libBuildInfo = (Cabal.libBuildInfo lib)
            { Cabal.cSources = clibVersionFile : Cabal.cSources (Cabal.libBuildInfo lib)
            }
        }
  let packageDesc' = packageDesc
        { Cabal.library = fmap updateLibrary (Cabal.library packageDesc)
        }
  builder
    (BuildActionBuild BuildModeDebug) -- TODO choose build type based on optimization level
    (BuildDirs cProjRoot (buildDir ++ "clibbuild") buildDir) -- TODO use some unique build dir name
  simpleBuildHook packageDesc' localBuildInfo userHooks buildFlags
  removeFile clibVersionFile

simpleBuildHook :: Cabal.PackageDescription
                -> Cabal.LocalBuildInfo
                -> Cabal.UserHooks
                -> Cabal.BuildFlags
                -> IO ()
simpleBuildHook = Cabal.buildHook Cabal.simpleUserHooks
