{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Execution (executeScript, executeWithRetries) where

import GHC.Generics (Generic)

import System.Process (readProcess)
import System.Info (os)
import Control.Exception (catch, SomeException)
import Data.Text (unpack)
import Types (Task(..), TaskInput(..), TaskOutput(..), ExecutionPlan(..))
import Monad(ExecutionMonad, getState, logMsg, updateState)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Filesystem (convertToDockerPath, makeAbsolutePath, joinPath, fileExists, takeExtension, takeFileName)


import Serialization (decodeJSON, extractJSONResult)

-- 🔹 Construye el ExecutionPlan a partir de una Task
buildExecutionPlan :: Task -> [TaskInput] -> Maybe ExecutionPlan
buildExecutionPlan task args =
    case (name task, command task, script task, taskToTaskOutput task) of
        (taskName, Just cmd, Nothing, outFile) -> Just (ExecutionPlan taskName cmd args outFile)
        (taskName, Nothing, Just file, outFile) -> Just (ExecutionPlan taskName file args outFile)
        _ -> Nothing

executeScript :: Task -> [TaskInput] -> ExecutionMonad (Either String TaskOutput)
executeScript task args = do
    logMsg $ "Preparando ejecución de tarea: " ++ name task
    case buildExecutionPlan task args of
        Just plan -> do
            manageContainer plan
        Nothing -> do
            logMsg "Error: Tarea mal definida, debe tener `command` o `script`, pero no ambos."
            return $ Left "Tarea mal definida"


executeWithRetries :: Task -> [TaskInput] -> Int -> ExecutionMonad (Either String TaskOutput)
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


-- Función auxiliar para Windows
toDockerWindowsPath :: FilePath -> FilePath
toDockerWindowsPath path =
    let drive = map toLower (take 1 path)  -- Extrae la letra de unidad (Ej: "C")
        rest  = drop 2 path  -- Quita "C:\" dejando solo "\Users\..."
    in "/" ++ drive ++ map (\c -> if c == '\\' then '/' else c) rest  -- Convierte a "/c/Users/..."




manageContainer :: ExecutionPlan -> ExecutionMonad (Either String TaskOutput)
manageContainer plan = do
    let command = execCommand plan
    if takeExtension command `elem` [".py", ".js", ".sh", ".c"]
        then do
            let (dockerImage, interpreter) = selectDockerImage command
            -- Convertir rutas a absolutas y en formato Docker
            absoluteScriptPath <- liftIO $ makeAbsolutePath (joinPath "tasks" command)
            dockerScriptPath <- liftIO $ convertToDockerPath absoluteScriptPath  
            let containerName = taskName plan

            let inputs = execArgs plan 
            let fileInputs = [filePath | FileInput filePath <- inputs]
            logMsg $ "fileInputs: " ++ show fileInputs

            containerIdResult <- liftIO $ runCommand 
                ["docker", "create", "--rm=false", "--name", containerName, "-v", dockerScriptPath ++ ":/app/" ++ takeFileName command, dockerImage, "sh", "-c", "sleep infinity"]

            case containerIdResult of
                Left err -> do
                    logMsg $ "Error creando contenedor: " ++ err
                    return $ Left err
                Right containerId -> do
                    logMsg $ "Contenedor creado con ID: " ++ containerId

                    _ <- liftIO $ mapM (\file -> do
                        let cpCommand = ["docker", "cp", file, containerId ++ ":/app/" ++ takeFileName file]
                        liftIO $ putStrLn $ "Ejecutando: " ++ unwords cpCommand
                        runCommand cpCommand
                        ) fileInputs

                    -- 🔹 Paso 3: Iniciar el contenedor
                    _ <- liftIO $ runCommand ["docker", "start", containerId]
                    logMsg $ "Contenedor iniciado: " ++ containerId

                    -- 🔹 Paso 4: Ejecutar el script dentro del contenedor
                    let args = [getInputValue input | input <- inputs]
                    let scriptExecutionCmd = interpreter ++ " /app/" ++ takeFileName command ++ " " ++ unwords args
                    logMsg $ "Ejecutando script en contenedor: docker exec -i " ++ containerId ++ " sh -c " ++ show scriptExecutionCmd

                    execResult <- liftIO $ runCommand  ["docker", "exec", "-i", containerId, "sh", "-c", scriptExecutionCmd]

                    case execResult of
                        Left err -> do
                            logMsg $ "Error ejecutando script en contenedor: " ++ err
                            return $ Left err
                        Right logs -> do
                            logMsg $ "Output del contenedor: " ++ logs

                            -- 🔹 Paso 5: Capturar el resultado y procesarlo
                            result <- processExecutionResult plan containerId logs

                            -- 🔹 Paso 6: Eliminar el contenedor después de la ejecución (en caso de éxito)
                            _ <- liftIO $ runCommand ["docker", "rm", "-f", containerId]
                            logMsg $ "Contenedor eliminado: " ++ containerId

                            return result

        else do

            let fullCmd = ["sh", "-c", command]

            logMsg $ "Comando: " ++ unwords fullCmd
        
            result <- liftIO $ runCommand fullCmd
            let expectedOutput = execOutput plan 

            case result of
                Left err -> do
                    logMsg $ "Error ejecutando comando: " ++ err
                    return $ Left err
                Right output -> do
                    logMsg $ "Output del comando: " ++ output

                    case expectedOutput of
                        OutputFile filePath -> do
                            logMsg $ "La tarea especifica un archivo de salida: " ++ filePath
                            return $ Right (OutputFile filePath)
                        OutputValue _ -> do
                            logMsg "La tarea especifica una variable de salida."
                            return $ Right (OutputValue output)


getInputValue :: TaskInput -> String
getInputValue (FileInput file) = "/app/" ++ takeFileName file  -- Se pasa la ruta del archivo dentro del contenedor
getInputValue (VarInput var)   = var  -- Se pasa directamente el valor si es una variable


taskToTaskOutput :: Task -> TaskOutput
taskToTaskOutput task = buildTaskOutput (output task)

buildTaskOutput :: Maybe String -> TaskOutput
buildTaskOutput (Just output) = OutputFile output
buildTaskOutput Nothing       = OutputValue ""


processExecutionResult :: ExecutionPlan -> String -> String -> ExecutionMonad (Either String TaskOutput)
processExecutionResult plan containerId logs = do 
    logMsg $ "Procesando resultado: " ++ logs
    logMsg $ "ExecOutput: " ++ show (execOutput plan)

    case execOutput plan of
        OutputFile filePath -> do
            logMsg $ "Copiando archivo de salida desde el contenedor: " ++ filePath
            let hostPath = "./output/" ++ takeFileName filePath
            let cpCommand = ["docker", "cp", containerId ++ ":/" ++ takeFileName filePath, hostPath]
            logMsg $ "Archivo guardado en: " ++ hostPath
            _ <- liftIO $ runCommand cpCommand
            return $ Right (OutputFile hostPath)

        OutputValue _ -> do
            logMsg "Procesando OutputValue"
            case extractJSONResult logs of
                Just result -> return $ Right (OutputValue result)
                Nothing     -> return $ Left "Error: la salida de la tarea no es un JSON válido con { result: x }"



runCommand :: [String] -> IO (Either String String)
runCommand args = catch
    (do
        let firstArg = head args
        let restArgs = tail args
        output <- readProcess firstArg restArgs ""
        let trimmedOutput = takeWhile (/= '\n') output  -- 🔹 Elimina el salto de línea
        return $ Right trimmedOutput)  -- 🔹 Devuelve el container ID limpio
    (\e -> return $ Left $ "Error ejecutando comando en Docker: " ++ show (e :: SomeException))
