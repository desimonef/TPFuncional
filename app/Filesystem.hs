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
  , textToFilePath
  , resolveInputPath
  ) where

import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Info (os)
import qualified Data.Text as T
import Data.Char (toLower)

createDirectoryIfMissingSafe :: FilePath -> IO ()
createDirectoryIfMissingSafe = createDirectoryIfMissing True

writeLazyFile :: FilePath -> BL.ByteString -> IO ()
writeLazyFile = BL.writeFile

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath = makeAbsolute

joinPath :: FilePath -> FilePath -> FilePath
joinPath = (</>)

textToFilePath :: T.Text -> FilePath
textToFilePath = T.unpack

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

resolveInputPath :: FilePath -> IO (Either String FilePath)
resolveInputPath path = do
  exists <- fileExists path
  if exists
    then return $ Right path
    else return $ Left $ "Archivo no encontrado: " ++ path
