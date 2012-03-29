module Paths_gnustep_config (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mokus/.cabal/bin"
libdir     = "/home/mokus/.cabal/lib/gnustep-config-0.1/ghc-7.4.1"
datadir    = "/home/mokus/.cabal/share/gnustep-config-0.1"
libexecdir = "/home/mokus/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "gnustep_config_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gnustep_config_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gnustep_config_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gnustep_config_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
