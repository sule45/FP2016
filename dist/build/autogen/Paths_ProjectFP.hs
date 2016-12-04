{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ProjectFP (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\cygwin64\\home\\msuko\\ProjectFP\\.cabal-sandbox\\bin"
libdir     = "C:\\cygwin64\\home\\msuko\\ProjectFP\\.cabal-sandbox\\x86_64-windows-ghc-8.0.1\\ProjectFP-0.1.0.0-HPbFlbptKFU8D8UkEgFzKx"
datadir    = "C:\\cygwin64\\home\\msuko\\ProjectFP\\.cabal-sandbox\\x86_64-windows-ghc-8.0.1\\ProjectFP-0.1.0.0"
libexecdir = "C:\\cygwin64\\home\\msuko\\ProjectFP\\.cabal-sandbox\\ProjectFP-0.1.0.0-HPbFlbptKFU8D8UkEgFzKx"
sysconfdir = "C:\\cygwin64\\home\\msuko\\ProjectFP\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProjectFP_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProjectFP_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ProjectFP_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProjectFP_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProjectFP_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
