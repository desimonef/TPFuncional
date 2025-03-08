{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Execution (executeScript) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, Value(..), (.:?), parseJSON)
import System.Process (readProcess)
import System.FilePath (takeExtension)
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Data.Text (unpack)
import Types (Task(..), ExecutionState(..), TaskOutput(..))

executeScript :: Task -> [String] -> IO (Either String TaskOutput)
executeScript task args = catch
    (do
        putStrLn $ "Ejecutando tarea: " ++ name task
        case (command task, script task, output task) of
            (Just cmd, Nothing, Just (OutputFile file)) -> do
                output <- readProcess cmd args ""
                writeFile file output
                putStrLn $ "Archivo generado: " ++ file
                return $ Right (OutputFile file)

            (Just cmd, Nothing, _) -> do
                output <- readProcess cmd args ""
                putStrLn output
                return $ Right OutputValue

            (Nothing, Just file, Just (OutputFile outFile)) -> do
                let fullCommand = if takeExtension file == ".bat"
                                  then ["cmd.exe", "/c", file] ++ args  -- 🔹 Ejecutar correctamente el .bat
                                  else [selectInterpreter file, file] ++ args
                output <- readProcess (head fullCommand) (tail fullCommand) ""  
                writeFile outFile output
                putStrLn $ "Archivo generado: " ++ outFile
                return $ Right (OutputFile outFile)

            (Nothing, Just file, _) -> do
                let fullCommand = if takeExtension file == ".bat"
                                  then ["cmd.exe", "/c", file] ++ args  -- 🔹 Ejecutar correctamente el .bat
                                  else [selectInterpreter file, file] ++ args
                output <- readProcess (head fullCommand) (tail fullCommand) ""  
                putStrLn output
                return $ Right OutputValue

            _ -> fail "Tarea mal definida: debe tener `command` o `script`, pero no ambos."
    )
    (\e -> return $ Left $ "Error ejecutando tarea " ++ name task ++ ": " ++ show (e :: SomeException))

-- Ejecuta un comando normal en la terminal
runCommand :: String -> [String] -> IO (Either String TaskOutput)
runCommand cmd args = do
    output <- readProcess cmd args ""
    putStrLn output
    return $ Right OutputValue

-- Detecta el intérprete correcto y ejecuta un script
runScript :: FilePath -> [String] -> IO (Either String TaskOutput)
runScript path args = do
    let interpreter = selectInterpreter path
    output <- readProcess interpreter (path : args) ""
    putStrLn output
    return $ Right OutputValue

executeFile :: FilePath -> [String] -> IO (Either String TaskOutput)
executeFile path args = catch
    (do
        let interpreter = selectInterpreter path
        output <- readProcess interpreter (path : args) ""
        putStrLn $ "Archivo generado: " ++ path  -- 🔹 Debug: Ver si el archivo se crea
        return $ Right OutputValue
    )
    (\e -> return $ Left $ "Error ejecutando archivo " ++ path ++ ": " ++ show (e :: SomeException))


-- Ejecuta un comando normal en la terminal
executeCommand :: String -> [String] -> IO (Either String TaskOutput)
executeCommand cmd args = catch
    (do
        output <- readProcess cmd args ""
        putStrLn output  -- 🔹 Ahora imprime la salida del comando en la terminal
        return $ Right OutputValue
    )
    (\e -> return $ Left $ "Error ejecutando comando " ++ cmd ++ ": " ++ show (e :: SomeException))

selectInterpreter :: FilePath -> String
selectInterpreter path =
    case takeExtension path of
        ".py"  -> "python3"
        ".js"  -> "node"
        ".sh"  -> "bash"
        ".bat" -> "cmd.exe"  -- 🔹 Para Windows
        _      -> error $ "No se reconoce el tipo de archivo: " ++ path


-- Elimina el salto de línea final del output
stripNewline :: String -> String
stripNewline = reverse . dropWhile (== '\n') . reverse
