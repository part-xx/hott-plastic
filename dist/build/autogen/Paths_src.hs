module Paths_src (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tao/.cabal/bin"
libdir     = "/home/tao/.cabal/lib/src-0.1/ghc-6.12.1"
datadir    = "/home/tao/.cabal/share/src-0.1"
libexecdir = "/home/tao/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "src_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "src_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "src_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "src_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
