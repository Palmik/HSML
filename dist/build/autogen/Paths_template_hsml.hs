module Paths_template_hsml (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/palmik/.cabal/bin"
libdir     = "/home/palmik/.cabal/lib/template-hsml-0.2.0.0/ghc-7.4.2"
datadir    = "/home/palmik/.cabal/share/template-hsml-0.2.0.0"
libexecdir = "/home/palmik/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "template_hsml_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "template_hsml_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "template_hsml_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "template_hsml_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
