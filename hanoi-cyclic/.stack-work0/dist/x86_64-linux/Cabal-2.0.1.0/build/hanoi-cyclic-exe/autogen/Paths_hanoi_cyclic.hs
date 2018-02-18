{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hanoi_cyclic (
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

bindir     = "/home/tom/Documents/fyp/hanoi-cyclic/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/bin"
libdir     = "/home/tom/Documents/fyp/hanoi-cyclic/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/lib/x86_64-linux-ghc-8.2.2/hanoi-cyclic-0.1.0.0-A0kLqh43m0n77gqQwRrqNS-hanoi-cyclic-exe"
dynlibdir  = "/home/tom/Documents/fyp/hanoi-cyclic/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/tom/Documents/fyp/hanoi-cyclic/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/share/x86_64-linux-ghc-8.2.2/hanoi-cyclic-0.1.0.0"
libexecdir = "/home/tom/Documents/fyp/hanoi-cyclic/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/libexec/x86_64-linux-ghc-8.2.2/hanoi-cyclic-0.1.0.0"
sysconfdir = "/home/tom/Documents/fyp/hanoi-cyclic/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hanoi_cyclic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hanoi_cyclic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hanoi_cyclic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hanoi_cyclic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hanoi_cyclic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hanoi_cyclic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
