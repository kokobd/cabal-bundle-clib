{-# LANGUAGE RecordWildCards #-}

module Development.CabalBundleCLib.CMake
  ( simpleCMakeBuilder
  ) where

import           Development.CabalBundleCLib.Types
import           System.Directory                  (removeDirectoryRecursive)
import           System.FilePath                   ((</>))
import           System.Process                    (callCommand, callProcess)

simpleCMakeBuilder :: BuildAction -> BuildDirs -> IO ()
simpleCMakeBuilder BuildActionClean BuildDirs{..} =
  removeDirectoryRecursive buildDirsBuild
simpleCMakeBuilder (BuildActionBuild mode) BuildDirs{..} = do
  callProcess "cmake" ["-S", buildDirsSource, "-B", buildDirsBuild]
  callProcess "cmake" ["--build", buildDirsBuild]
  callCommand $ "cp " ++ buildDirsBuild </> "*.a" ++ " " ++ buildDirsInstall

{-
cmake -S $SOURCE_DIR -B $BUILD_DIR
cmake --build $BUILD_DIR
cp $BUILD_DIR/*.a $BUILD_DIR/*.so.* $INSTALL_DIR
-}
