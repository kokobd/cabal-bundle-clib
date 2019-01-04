module Development.CabalBundleCLib
  ( mainWithCLib
  , module Development.CabalBundleCLib.Types
  ) where

import           Data.Time.Clock                            (getCurrentTime)
import           Development.CabalBundleCLib.Types
import qualified Distribution.Compat.Lens                   as Lens
import qualified Distribution.PackageDescription            as Cabal hiding
                                                                      (Flag)
import qualified Distribution.Simple                        as Cabal
import qualified Distribution.Simple.Setup                  as Cabal
import qualified Distribution.Types.BuildInfo.Lens          as CabalLens
import qualified Distribution.Types.Library.Lens            as CabalLens
import qualified Distribution.Types.LocalBuildInfo          as Cabal
import qualified Distribution.Types.PackageDescription.Lens as CabalLens
import           System.Directory                           (removeFile)
import           System.FilePath                            ((</>))
import           System.IO.Temp                             (writeTempFile)

mainWithCLib :: FilePath -- ^c project root
             -> [String] -- ^bundled libraries
             -> [FilePath] -- ^parent dirs of libraries (relative to build root)
             -> (BuildAction -> BuildDirs -> IO ()) -- ^builder
             -- The three 'FilePath's are source dir, build dir and installation
             -- dir, respectively
             -> IO ()
mainWithCLib cProjRoot bundledLibs bundledLibDirs builder =
  Cabal.defaultMainWithHooks Cabal.simpleUserHooks
    { Cabal.confHook = customConfigure bundledLibs
    , Cabal.postConf = \_ _ _ _ -> pure () -- remove the check for foreign libs
    , Cabal.buildHook = customBuild cProjRoot bundledLibs bundledLibDirs builder
    }

customConfigure :: [String]
                -> (Cabal.GenericPackageDescription, Cabal.HookedBuildInfo)
                -> Cabal.ConfigFlags
                -> IO Cabal.LocalBuildInfo
customConfigure bundledLibs gpkgDesc configFlags = do
  lbi <- Cabal.confHook Cabal.simpleUserHooks gpkgDesc configFlags
  let localPkgDescr = Cabal.localPkgDescr lbi
      localPkgDescr' = Lens.over CabalLens.library updateLibrary localPkgDescr
  pure $ lbi { Cabal.localPkgDescr = localPkgDescr' }
  where
    updateLibrary :: Maybe Cabal.Library -> Maybe Cabal.Library
    updateLibrary = fmap $ Lens.over CabalLens.libBuildInfo updateBuildInfo
    updateBuildInfo :: Cabal.BuildInfo -> Cabal.BuildInfo
    updateBuildInfo bi = bi
      { Cabal.extraLibs = bundledLibs ++ Cabal.extraLibs bi
      , Cabal.extraBundledLibs = bundledLibs ++ Cabal.extraBundledLibs bi
      }

customBuild :: FilePath -- ^c project root
            -> [String] -- ^bundled libs
            -> [FilePath] -- bundled lib dirs
            -> (BuildAction -> BuildDirs -> IO ())
            -> Cabal.PackageDescription
            -> Cabal.LocalBuildInfo
            -> Cabal.UserHooks
            -> Cabal.BuildFlags
            -> IO ()
customBuild cProjRoot bundledLibs bundledLibDirs builder packageDesc localBuildInfo userHooks buildFlags = do
  currentTime <- getCurrentTime
  let versionInfo = "const char *clibver = \"" ++ show currentTime ++ "\";\n"
  let buildDir = Cabal.buildDir localBuildInfo
  clibVersionFile <- writeTempFile buildDir "clibver.c" versionInfo
  let updateLibrary :: Cabal.Library -> Cabal.Library
      updateLibrary lib =
        let lib' = Lens.over (CabalLens.libBuildInfo . CabalLens.cSources) (clibVersionFile :) lib
            lib'' = Lens.over (CabalLens.libBuildInfo . CabalLens.extraLibDirs)
              ((fmap (\dir -> buildDir </> dir) bundledLibDirs) ++) lib'
         in Lens.over (CabalLens.libBuildInfo . CabalLens.extraLibs) (bundledLibs ++) lib''
  let packageDesc' = Lens.over CabalLens.library (fmap updateLibrary) packageDesc
  builder
    (BuildActionBuild (getBuildMode localBuildInfo))
    (BuildDirs cProjRoot (buildDir </> "clibbuild") buildDir) -- TODO use some unique build dir name
  simpleBuildHook packageDesc' localBuildInfo userHooks buildFlags
  removeFile clibVersionFile

getBuildMode :: Cabal.LocalBuildInfo -> BuildMode
getBuildMode localBuildInfo =
  case Cabal.configOptimization . Cabal.configFlags $ localBuildInfo of
    Cabal.Flag level ->
      case level of
        Cabal.MaximumOptimisation -> BuildModeRelease
        _                         -> BuildModeDebug
    _ -> BuildModeDebug

simpleBuildHook :: Cabal.PackageDescription
                -> Cabal.LocalBuildInfo
                -> Cabal.UserHooks
                -> Cabal.BuildFlags
                -> IO ()
simpleBuildHook = Cabal.buildHook Cabal.simpleUserHooks
