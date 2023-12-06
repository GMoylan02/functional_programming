{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_makex3 (
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

bindir     = "D:\\projects\\FPCW\\Makex3\\.stack-work\\install\\1a1856f4\\bin"
libdir     = "D:\\projects\\FPCW\\Makex3\\.stack-work\\install\\1a1856f4\\lib\\x86_64-windows-ghc-8.10.7\\makex3-0.1.0.0-LnaKgChFjgS49RfpmvG1pp"
dynlibdir  = "D:\\projects\\FPCW\\Makex3\\.stack-work\\install\\1a1856f4\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "D:\\projects\\FPCW\\Makex3\\.stack-work\\install\\1a1856f4\\share\\x86_64-windows-ghc-8.10.7\\makex3-0.1.0.0"
libexecdir = "D:\\projects\\FPCW\\Makex3\\.stack-work\\install\\1a1856f4\\libexec\\x86_64-windows-ghc-8.10.7\\makex3-0.1.0.0"
sysconfdir = "D:\\projects\\FPCW\\Makex3\\.stack-work\\install\\1a1856f4\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "makex3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "makex3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "makex3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "makex3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "makex3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "makex3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
