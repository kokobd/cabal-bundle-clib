module Development.CabalBundleCLib.Types
  ( BuildAction(..)
  , BuildMode(..)
  , BuildDirs(..)
  , Builder
  ) where

-- |Type for build actions.
data BuildAction =
    BuildActionBuild BuildMode
  | BuildActionClean
  deriving (Show, Eq)

data BuildMode = BuildModeDebug | BuildModeRelease
  deriving (Show, Eq)

data BuildDirs = BuildDirs
  { buildDirsSource  :: FilePath
  , buildDirsBuild   :: FilePath
  , buildDirsInstall :: FilePath
  } deriving (Show, Eq)

type Builder = BuildAction -> BuildDirs -> IO ()
