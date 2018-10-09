{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_telegram_api (
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
version = Version [0,7,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dan/.cabal/bin"
libdir     = "/home/dan/.cabal/lib/x86_64-linux-ghc-8.4.3/telegram-api-0.7.2.0-inplace"
dynlibdir  = "/home/dan/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/dan/.cabal/share/x86_64-linux-ghc-8.4.3/telegram-api-0.7.2.0"
libexecdir = "/home/dan/.cabal/libexec/x86_64-linux-ghc-8.4.3/telegram-api-0.7.2.0"
sysconfdir = "/home/dan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "telegram_api_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "telegram_api_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "telegram_api_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "telegram_api_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "telegram_api_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "telegram_api_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
