-- Filesystem.hs
module Filesystem
  ( createDirectoryIfMissingSafe
  , writeLazyFile
  , fileExists
  , makeAbsolutePath
  , joinPath
  , convertToDockerPath
  , takeExtension
  , takeFileName
  ) where

import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Process (readProcess)
import System.Info (os)
import Control.Exception (catch, SomeException)
import Data.Char (toLower)

-- Crear directorio si no existe
createDirectoryIfMissingSafe :: FilePath -> IO ()
createDirectoryIfMissingSafe = createDirectoryIfMissing True

-- Escribir archivo lazy
writeLazyFile :: FilePath -> BL.ByteString -> IO ()
writeLazyFile = BL.writeFile

-- Verifica existencia de archivo
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

-- Ruta absoluta
makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath = makeAbsolute

-- Unir paths
joinPath :: FilePath -> FilePath -> FilePath
joinPath = (</>)

-- Convertir path para Docker (Windows/Linux)
convertToDockerPath :: FilePath -> IO FilePath
convertToDockerPath path = do
  absPath <- makeAbsolutePath path
  return $ case os of
    "mingw32" -> toDockerWindowsPath absPath
    "cygwin"  -> toDockerWindowsPath absPath
    _         -> absPath

toDockerWindowsPath :: FilePath -> FilePath
toDockerWindowsPath path =
  let drive = map toLower (take 1 path)
      rest  = drop 2 path
  in "/" ++ drive ++ map (\c -> if c == '\\' then '/' else c) rest
