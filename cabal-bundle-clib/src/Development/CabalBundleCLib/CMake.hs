{-# LANGUAGE RecordWildCards #-}

module Development.CabalBundleCLib.CMake
  ( simpleCMakeBuilder
  ) where

import           Development.CabalBundleCLib.Types
import           System.Directory                  (withCurrentDirectory)
import           System.FilePath                   ((</>))
import           System.Process                    (callCommand, callProcess)

simpleCMakeBuilder :: BuildAction -> BuildDirs -> IO ()
simpleCMakeBuilder BuildActionClean BuildDirs{..} =
  withCurrentDirectory buildDirsBuild $
    callProcess "make" ["clean"]
simpleCMakeBuilder (BuildActionBuild mode) BuildDirs{..} = do
  callProcess "cmake" ["-S", buildDirsSource, "-B", buildDirsBuild, (buildModeToFlag mode)]
  callProcess "cmake" ["--build", buildDirsBuild]
  callCommand $ "cp " ++ buildDirsBuild </> "*.a" ++ " " ++ buildDirsInstall

buildModeToFlag :: BuildMode -> String
buildModeToFlag buildMode = "-DCMAKE_BUILD_TYPE=" ++ mode
  where
    mode = case buildMode of
             BuildModeDebug   -> "Debug"
             BuildModeRelease -> "Release"

{-
cmake -S $SOURCE_DIR -B $BUILD_DIR
cmake --build $BUILD_DIR
cp $BUILD_DIR/*.a $BUILD_DIR/*.so.* $INSTALL_DIR
-}
