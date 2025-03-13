{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Execution (executeScript, executeWithRetries) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, Value(..), (.:?), parseJSON, (.:), decode)
import System.Process (readProcess)
import System.FilePath (takeExtension)
import System.Directory (doesFileExist, makeAbsolute)
import Control.Exception (catch, SomeException)
import Data.Text (unpack)
import Types (Task(..), ExecutionState(..), TaskOutput(..), ExecutionPlan(..))
import Monad(ExecutionMonad, getState, logMsg, updateState)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as B

-- 🔹 Construye el ExecutionPlan a partir de una Task
buildExecutionPlan :: Task -> [String] -> Maybe ExecutionPlan
buildExecutionPlan task args =
    case (command task, script task, taskToTaskOutput task) of
        (Just cmd, Nothing, outFile) -> Just (ExecutionPlan cmd args outFile)
        (Nothing, Just file, outFile) -> Just (ExecutionPlan file args outFile)
        _ -> Nothing

executeScript :: Task -> [String] -> ExecutionMonad (Either String TaskOutput)
executeScript task args = do
    logMsg $ "Preparando ejecución de tarea: " ++ name task
    case buildExecutionPlan task args of
        Just plan -> do
            logMsg $ "Ejecutando: " ++ executionCommand plan
            manageContainer plan
        Nothing -> do
            logMsg "Error: Tarea mal definida, debe tener `command` o `script`, pero no ambos."
            return $ Left "Tarea mal definida"

executionCommand :: ExecutionPlan -> String
executionCommand (ExecutionPlan cmd args _) = cmd ++ " " ++ unwords args

executeWithRetries :: Task -> [String] -> Int -> ExecutionMonad (Either String TaskOutput)
executeWithRetries task args remainingRetries = do
    logMsg $ "Ejecutando tarea: " ++ name task
    result <- executeScript task args  

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

-- 🔹 Selecciona la imagen de Docker y el intérprete según el tipo de script o comando
selectDockerImage :: String -> (String, String)
selectDockerImage cmd
    | takeExtension cmd == ".py"  = ("python:3.9", "python")
    | takeExtension cmd == ".js"  = ("node:18", "node")
    | takeExtension cmd == ".sh"  = ("ubuntu:latest", "bash")
    | takeExtension cmd == ".c"   = ("gcc:latest", "gcc")
    | otherwise                   = ("ubuntu:latest", "sh")

convertToDockerPath :: String -> String
convertToDockerPath (d:':':xs) = '/' : toLower d : map (\c -> if c == '\\' then '/' else c) xs
convertToDockerPath path       = map (\c -> if c == '\\' then '/' else c) path

manageContainer :: ExecutionPlan -> ExecutionMonad (Either String TaskOutput)
manageContainer plan = do
    let command = execCommand plan
    let (dockerImage, interpreter) = selectDockerImage command

    if takeExtension command `elem` [".py", ".js", ".sh", ".c"]
        then do
            absoluteScriptPath <- liftIO $ makeAbsolute ("./tasks/" ++ command)
            let dockerScriptPath = convertToDockerPath absoluteScriptPath  
            
            let cleanArgs = map (filter (/= '\r') . filter (/= '\n')) (execArgs plan)
            let containerArgs = ["run", "-d", "--rm=false", "-v", dockerScriptPath ++ ":/app/" ++ command, dockerImage, "sh", "-c", "chmod +r /app/" ++ command ++ " && " ++ interpreter ++ " /app/" ++ command ++ " " ++ unwords cleanArgs ++ " && sleep infinity"]
            logMsg $ "Ejecutando script en Docker con args procesados: " ++ show (execArgs plan)

            logMsg $ "Ejecutando script en Docker: " ++ dockerImage ++ " " ++ unwords containerArgs
            containerIdResult <- liftIO $ runCommand containerArgs

            case containerIdResult of
                Left err -> do
                    logMsg $ "Error iniciando contenedor: " ++ err
                    return $ Left err
                Right containerId -> do
                    logMsg $ "Contenedor iniciado con ID: " ++ containerId

                    -- 🔹 Capturar logs del contenedor
                    logsResult <- liftIO $ runCommand ["logs", containerId]

                    -- 🔹 Procesar resultado antes de eliminar el contenedor
                    result <- processExecutionResult plan containerId logsResult

                    -- 🔹 Eliminar el contenedor después de copiar los archivos
                    --_ <- liftIO $ runCommand ["docker", "rm", "-f", containerId]

                    return result

        else do
            let containerArgs = ["run", "-d", "-v", convertToDockerPath "./tasks" ++ ":/tasks", dockerImage, "sh", "-c", command] ++ execArgs plan

            logMsg $ "Ejecutando comando en Docker: " ++ dockerImage ++ " " ++ unwords containerArgs
            containerIdResult <- liftIO $ runCommand containerArgs

            case containerIdResult of
                Left err -> do
                    logMsg $ "Error iniciando contenedor: " ++ err
                    return $ Left err
                Right containerId -> do
                    logMsg $ "Contenedor iniciado con ID: " ++ containerId

                    logsResult <- liftIO $ runCommand ["logs", containerId]
                    _ <- liftIO $ runCommand ["rm", "-f", containerId]

                    processExecutionResult plan containerId logsResult

extractJSONResult :: String -> Maybe String
extractJSONResult output =
    case dropWhile (/= ':') output of  -- Buscamos el primer ':'
        (':':'"':rest) -> Just (takeWhile (/= '"') rest)  -- Extraemos hasta la siguiente '"'
        _ -> Nothing  -- No se encontró el formato esperado

taskToTaskOutput :: Task -> TaskOutput
taskToTaskOutput task = buildTaskOutput (output task)

buildTaskOutput :: Maybe String -> TaskOutput
buildTaskOutput (Just output) = OutputFile output
buildTaskOutput Nothing       = OutputValue ""

processExecutionResult :: ExecutionPlan -> String -> Either String String -> ExecutionMonad (Either String TaskOutput)
processExecutionResult plan containerId (Left err) = return $ Left err
processExecutionResult plan containerId (Right logs) = do 
    logMsg $ "logs del contenedor: " ++ logs
    logMsg $ "ExecOutput: " ++ show (execOutput plan)

    case execOutput plan of
        -- 🔹 Si el output es un archivo, copiarlo al host
        OutputFile filePath -> do
            logMsg $ "Copiando archivo desde el contenedor: " ++ filePath
            let hostPath = "./output/" ++ filePath  -- 🔹 Ruta donde queremos guardarlo en el host

            -- 🔹 Comando para copiar el archivo desde el contenedor específico al host
            let cpCommand = ["cp", containerId ++ ":" ++ filePath, hostPath]
            copyResult <- liftIO $ runCommand cpCommand

            logMsg $ "Ejecutando: " ++ unwords cpCommand  -- 🔹 Log para verificar qué se ejecuta

            case copyResult of
                Left err -> return $ Left ("Error copiando archivo: " ++ err)
                Right _  -> do
                    logMsg "Éxito al copiar el archivo"
                    return $ Right (OutputFile hostPath)

        -- 🔹 Si es un OutputValue, asumimos que la salida es JSON y extraemos `x`
        OutputValue _ -> do
            logMsg "Procesando OutputValue"
            case extractJSONResult logs of
                Just result -> return $ Right (OutputValue result)
                Nothing     -> return $ Left "Error: la salida de la tarea no es un JSON válido con { result: x }"



runCommand :: [String] -> IO (Either String String)
runCommand args = catch
    (do
        output <- readProcess "docker" args ""
        let trimmedOutput = takeWhile (/= '\n') output  -- 🔹 Elimina el salto de línea
        return $ Right trimmedOutput)  -- 🔹 Devuelve el container ID limpio
    (\e -> return $ Left $ "Error ejecutando comando en Docker: " ++ show (e :: SomeException))
