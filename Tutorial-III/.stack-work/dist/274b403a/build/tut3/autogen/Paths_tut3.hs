{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tut3 (
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

bindir     = "D:\\projects\\FPCW\\Tutorial-III\\.stack-work\\install\\1a1856f4\\bin"
libdir     = "D:\\projects\\FPCW\\Tutorial-III\\.stack-work\\install\\1a1856f4\\lib\\x86_64-windows-ghc-8.10.7\\tut3-0.1.0.0-8NoYaO81Uoz1N8EKKzf7Uz-tut3"
dynlibdir  = "D:\\projects\\FPCW\\Tutorial-III\\.stack-work\\install\\1a1856f4\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "D:\\projects\\FPCW\\Tutorial-III\\.stack-work\\install\\1a1856f4\\share\\x86_64-windows-ghc-8.10.7\\tut3-0.1.0.0"
libexecdir = "D:\\projects\\FPCW\\Tutorial-III\\.stack-work\\install\\1a1856f4\\libexec\\x86_64-windows-ghc-8.10.7\\tut3-0.1.0.0"
sysconfdir = "D:\\projects\\FPCW\\Tutorial-III\\.stack-work\\install\\1a1856f4\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tut3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tut3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tut3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tut3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tut3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tut3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
