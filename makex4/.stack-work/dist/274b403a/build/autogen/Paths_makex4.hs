{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_makex4 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\projects\\FPCW\\makex4\\.stack-work\\install\\1a1856f4\\bin"
libdir     = "D:\\projects\\FPCW\\makex4\\.stack-work\\install\\1a1856f4\\lib\\x86_64-windows-ghc-8.10.7\\makex4-0.1.0.0-D5fU4ZJY5MgGmDFAfc8DBR"
dynlibdir  = "D:\\projects\\FPCW\\makex4\\.stack-work\\install\\1a1856f4\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "D:\\projects\\FPCW\\makex4\\.stack-work\\install\\1a1856f4\\share\\x86_64-windows-ghc-8.10.7\\makex4-0.1.0.0"
libexecdir = "D:\\projects\\FPCW\\makex4\\.stack-work\\install\\1a1856f4\\libexec\\x86_64-windows-ghc-8.10.7\\makex4-0.1.0.0"
sysconfdir = "D:\\projects\\FPCW\\makex4\\.stack-work\\install\\1a1856f4\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "makex4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "makex4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "makex4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "makex4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "makex4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "makex4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
