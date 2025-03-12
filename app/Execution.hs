{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Execution (executeScript, executeWithRetries) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, Value(..), (.:?), parseJSON)
import System.Process (readProcess)
import System.FilePath (takeExtension)
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Data.Text (unpack)
import Types (Task(..), ExecutionState(..), TaskOutput(..), ExecutionPlan(..))
import Monad(ExecutionMonad, getState, logMsg, updateState)
import Control.Monad.IO.Class (liftIO)

executeScript :: Task -> [String] -> ExecutionMonad (Either String TaskOutput)
executeScript task args = do
    logMsg $ "Preparando ejecución de tarea: " ++ name task
    case (command task, script task, output task) of
        (Just cmd, Nothing, outFile) -> do
            let plan = ExecutionPlan cmd args outFile  -- ✅ No extraemos la ruta, usamos TaskOutput directamente
            logMsg $ "Comando a ejecutar: " ++ cmd ++ " " ++ unwords args
            liftIO $ runCommand plan

        (Nothing, Just file, outFile) -> do
            let fullCommand = if takeExtension file == ".bat" then "cmd.exe" else selectInterpreter file
                fullArgs = if takeExtension file == ".bat" then ["/c", file] ++ args else [file] ++ args
                plan = ExecutionPlan fullCommand fullArgs outFile  -- ✅ Sin conversión innecesaria
            logMsg $ "Script a ejecutar: " ++ fullCommand ++ " " ++ unwords fullArgs
            liftIO $ runCommand plan

        _ -> do
            logMsg "Error: Tarea mal definida, debe tener `command` o `script`, pero no ambos."
            return $ Left "Tarea mal definida"



executeWithRetries :: Task -> [String] -> Int -> ExecutionMonad (Either String TaskOutput)
executeWithRetries task args remainingRetries = do
    logMsg $ "Ejecutando tarea: " ++ name task
    result <- executeScript task args  -- 🔹 Ahora es PURA (en `ExecutionMonad`)

    case result of
        Right output -> do
            logMsg $ "Tarea " ++ name task ++ " ejecutada exitosamente."
            return $ Right output
        Left err -> 
            if remainingRetries > 0 then do
                logMsg $ "Error en tarea " ++ name task ++ ": " ++ err
                logMsg $ "Reintentando tarea " ++ name task ++ "... (" ++ show remainingRetries ++ " intentos restantes)"
                executeWithRetries task args (remainingRetries - 1)
            else do
                logMsg $ "Error crítico en tarea " ++ name task ++ ", no hay más intentos disponibles."
                return $ Left err


selectInterpreter :: FilePath -> String
selectInterpreter path =
    case takeExtension path of
        ".py"  -> "python3"
        ".js"  -> "node"
        ".sh"  -> "bash"
        ".bat" -> "cmd.exe"  -- 🔹 Para Windows
        _      -> error $ "No se reconoce el tipo de archivo: " ++ path

extractOutputPath :: Maybe TaskOutput -> Maybe String
extractOutputPath (Just (OutputFile path)) = Just path
extractOutputPath _ = Nothing

runCommand :: ExecutionPlan -> IO (Either String TaskOutput)
runCommand (ExecutionPlan cmd args outputFile) = catch
    (do
        putStrLn $ "Ejecutando: " ++ cmd ++ " " ++ unwords args
        output <- readProcess cmd args ""
        case outputFile of
            Just (OutputFile file) -> do  -- ✅ Directamente usa `TaskOutput`
                writeFile file output
                putStrLn $ "Archivo generado: " ++ file
                return $ Right (OutputFile file)
            Nothing -> do
                putStrLn output
                return $ Right (OutputValue output)
    )
    (\e -> return $ Left $ "Error ejecutando comando: " ++ show (e :: SomeException))

